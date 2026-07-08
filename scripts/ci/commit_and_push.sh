#!/usr/bin/env bash

set -e

dry_run=0
commit_message=""
no_changes_message="No changes to commit."
target_branch_error_message="Could not determine target branch."
stage_paths=()
stage_if_exists_paths=()
conflict_paths=()

print_cmd() {
  printf '+'
  for arg in "$@"; do
    printf ' %q' "$arg"
  done
  printf '\n'
}

run_cmd() {
  print_cmd "$@"
  if [ "$dry_run" -eq 0 ]; then
    "$@"
  fi
}

run_cmd_or_true() {
  print_cmd "$@"
  if [ "$dry_run" -eq 0 ]; then
    "$@" || true
  fi
}

path_matches() {
  local candidate="$1"
  local allowed="$2"

  case "$allowed" in
    */)
      case "$candidate" in
        "$allowed"*) return 0 ;;
      esac
      ;;
    *)
      if [ "$candidate" = "$allowed" ]; then
        return 0
      fi
      ;;
  esac

  return 1
}

path_matches_any() {
  local candidate="$1"
  shift
  local allowed
  for allowed in "$@"; do
    if path_matches "$candidate" "$allowed"; then
      return 0
    fi
  done
  return 1
}

while [ $# -gt 0 ]; do
  case "$1" in
    --commit-message)
      commit_message="${2:-}"
      shift 2
      ;;
    --no-changes-message)
      no_changes_message="${2:-}"
      shift 2
      ;;
    --target-branch-error-message)
      target_branch_error_message="${2:-}"
      shift 2
      ;;
    --add)
      stage_paths+=("${2:-}")
      shift 2
      ;;
    --add-if-exists)
      stage_if_exists_paths+=("${2:-}")
      shift 2
      ;;
    --conflict-path)
      conflict_paths+=("${2:-}")
      shift 2
      ;;
    --dry-run)
      dry_run=1
      shift
      ;;
    *)
      echo "Unknown argument: $1"
      exit 1
      ;;
  esac
done

if [ -z "$commit_message" ]; then
  echo "Missing required --commit-message argument."
  exit 1
fi

if [ "${#stage_paths[@]}" -eq 0 ]; then
  echo "At least one --add path is required."
  exit 1
fi

if [ "${#conflict_paths[@]}" -eq 0 ]; then
  conflict_paths=("${stage_paths[@]}")
fi

run_cmd git config user.name "github-actions[bot]"
run_cmd git config user.email "41898282+github-actions[bot]@users.noreply.github.com"

run_cmd_or_true git add -f "${stage_paths[@]}"

for path in "${stage_if_exists_paths[@]}"; do
  if [ "$dry_run" -eq 1 ]; then
    echo "if [ -f $path ]; then"
    print_cmd git add -f "$path"
    echo "fi"
  elif [ -f "$path" ]; then
    run_cmd git add -f "$path"
  fi
done

if [ "$dry_run" -eq 1 ]; then
  echo "Dry run: printing git commands without executing them."
  echo "+ git diff --staged --quiet"
  echo "  -> if no staged changes: $no_changes_message"
  print_cmd git commit -m "$commit_message"
  echo "+ git stash 2>/dev/null || true"
  target_branch="${GITHUB_HEAD_REF:-${GITHUB_REF_NAME:-<current-branch>}}"
  remote_ref="origin/$target_branch"
  echo "Dry run target branch: $remote_ref"
  for attempt in 1 2 3; do
    echo "Push attempt $attempt/3"
    print_cmd git fetch origin "$target_branch"
    print_cmd git rebase "$remote_ref"
    echo "  -> if conflicts remain on configured paths, run:"
    for path in "${conflict_paths[@]}"; do
      echo "+ git checkout --theirs -- $path 2>/dev/null || true"
    done
    print_cmd git add "${conflict_paths[@]}"
    echo "+ git rebase --continue"
    print_cmd git push origin "HEAD:$target_branch"
  done
  echo "Dry run complete."
  exit 0
fi

if git diff --staged --quiet; then
  echo "$no_changes_message"
  exit 0
fi

git commit -m "$commit_message"

git stash 2>/dev/null || true
target_branch="${GITHUB_HEAD_REF:-${GITHUB_REF_NAME:-}}"
if [ -z "$target_branch" ] || [ "$target_branch" = "HEAD" ]; then
  target_branch=$(git rev-parse --abbrev-ref HEAD)
fi
if [ -z "$target_branch" ] || [ "$target_branch" = "HEAD" ]; then
  echo "$target_branch_error_message"
  exit 1
fi

remote_ref="origin/$target_branch"
echo "Publishing commit to $remote_ref"

for attempt in 1 2 3; do
  echo "Push attempt $attempt/3"
  git fetch origin "$target_branch"
  if git rebase "$remote_ref"; then
    :
  else
    conflicted=$(git diff --name-only --diff-filter=U || true)
    if [ -n "$conflicted" ]; then
      echo "Conflicts on: $conflicted"
      for path in "${conflict_paths[@]}"; do
        git checkout --theirs -- "$path" 2>/dev/null || true
      done

      still_conflicted=""
      while IFS= read -r path; do
        [ -n "$path" ] || continue
        if ! path_matches_any "$path" "${conflict_paths[@]}"; then
          if [ -z "$still_conflicted" ]; then
            still_conflicted="$path"
          else
            still_conflicted="$still_conflicted $path"
          fi
        fi
      done <<EOF
$conflicted
EOF

      if [ -n "$still_conflicted" ]; then
        echo "Non-data conflicts remain; aborting rebase: $still_conflicted"
        git rebase --abort
        exit 1
      fi

      git add "${conflict_paths[@]}" 2>/dev/null || true
      git rebase --continue
    else
      git rebase --abort 2>/dev/null || true
      exit 1
    fi
  fi

  if git push origin "HEAD:$target_branch"; then
    echo "Push succeeded on attempt $attempt"
    exit 0
  fi

  echo "Push attempt $attempt rejected, retrying after a short backoff"
  sleep $((attempt * 5))
done

echo "All push attempts failed."
exit 1
