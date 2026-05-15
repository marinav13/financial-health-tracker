const REVIEW_APPROVED_STATUS = 'approved';
const DIRTY_FLAG_PROPERTY = 'ACCREDITATION_REVIEW_DIRTY';
const DIRTY_REASON_PROPERTY = 'ACCREDITATION_REVIEW_DIRTY_REASON';
const DIRTY_AT_PROPERTY = 'ACCREDITATION_REVIEW_DIRTY_AT';
const LAST_DISPATCH_AT_PROPERTY = 'ACCREDITATION_REVIEW_LAST_DISPATCH_AT';
const LAST_DISPATCH_STATUS_PROPERTY = 'ACCREDITATION_REVIEW_LAST_DISPATCH_STATUS';
const LAST_DISPATCH_RESPONSE_PROPERTY = 'ACCREDITATION_REVIEW_LAST_DISPATCH_RESPONSE';

const PUBLISH_TRIGGER_HEADERS = [
  'review_status',
  'editor_action_label_short',
  'editor_action_date',
  'editor_action_type',
  'editor_source_url',
  'editor_source_title'
];

const DEFAULT_SETTINGS = {
  sheetTabName: 'accreditation_review',
  eventType: 'accreditation_review_publish',
  dispatchIntervalMinutes: 15
};

function installTriggers() {
  const spreadsheet = SpreadsheetApp.getActiveSpreadsheet();
  if (!spreadsheet) {
    throw new Error('installTriggers() must run from the bound spreadsheet project.');
  }

  const intervalMinutes = normalizeDispatchInterval_(
    getSettings_().dispatchIntervalMinutes
  );

  ScriptApp.getProjectTriggers().forEach(function(trigger) {
    const handler = trigger.getHandlerFunction();
    if (handler === 'onAccreditationReviewEdit' || handler === 'dispatchAccreditationReviewIfDirty') {
      ScriptApp.deleteTrigger(trigger);
    }
  });

  ScriptApp.newTrigger('onAccreditationReviewEdit')
    .forSpreadsheet(spreadsheet)
    .onEdit()
    .create();

  ScriptApp.newTrigger('dispatchAccreditationReviewIfDirty')
    .timeBased()
    .everyMinutes(intervalMinutes)
    .create();
}

function onAccreditationReviewEdit(e) {
  if (!e || !e.range) {
    return;
  }

  const settings = getSettings_();
  const sheet = e.range.getSheet();
  if (!sheet || sheet.getName() !== settings.sheetTabName) {
    return;
  }

  if (e.range.getRow() <= 1) {
    return;
  }

  const headerMap = getHeaderMap_(sheet);
  const touchedHeaders = getTouchedHeaders_(e.range, headerMap);
  const touchedPublishHeaders = touchedHeaders.filter(function(header) {
    return PUBLISH_TRIGGER_HEADERS.indexOf(header) !== -1;
  });

  if (!touchedPublishHeaders.length) {
    return;
  }

  const rowValues = sheet
    .getRange(e.range.getRow(), 1, 1, sheet.getLastColumn())
    .getDisplayValues()[0];
  const reviewStatus = getRowValueByHeader_(rowValues, headerMap, 'review_status');
  const normalizedReviewStatus = normalizeCellValue_(reviewStatus);
  const touchedReviewStatus = touchedPublishHeaders.indexOf('review_status') !== -1;

  if (!touchedReviewStatus && normalizedReviewStatus !== REVIEW_APPROVED_STATUS) {
    return;
  }

  const actionId = getRowValueByHeader_(rowValues, headerMap, 'action_id');
  const reason = [
    'approved row edited',
    normalizeCellValue_(actionId) || '(missing action_id)',
    touchedPublishHeaders.join(', ')
  ].join(' | ');

  markAccreditationReviewDirty_(reason);
}

function dispatchAccreditationReviewIfDirty() {
  const lock = LockService.getScriptLock();
  lock.waitLock(30000);

  try {
    const properties = PropertiesService.getScriptProperties();
    if (properties.getProperty(DIRTY_FLAG_PROPERTY) !== 'true') {
      return;
    }

    const settings = getSettings_();
    const payload = {
      event_type: settings.eventType,
      client_payload: {
        dirty_at: properties.getProperty(DIRTY_AT_PROPERTY) || '',
        dirty_reason: properties.getProperty(DIRTY_REASON_PROPERTY) || '',
        sheet_tab: settings.sheetTabName,
        source: 'google_apps_script'
      }
    };

    const response = UrlFetchApp.fetch(
      'https://api.github.com/repos/' + settings.githubOwner + '/' + settings.githubRepo + '/dispatches',
      {
        method: 'post',
        contentType: 'application/json',
        muteHttpExceptions: true,
        headers: {
          Accept: 'application/vnd.github+json',
          Authorization: 'Bearer ' + settings.githubToken,
          'X-GitHub-Api-Version': '2022-11-28'
        },
        payload: JSON.stringify(payload)
      }
    );

    const responseCode = response.getResponseCode();
    const responseBody = response.getContentText();
    properties.setProperty(LAST_DISPATCH_AT_PROPERTY, new Date().toISOString());
    properties.setProperty(LAST_DISPATCH_STATUS_PROPERTY, String(responseCode));
    properties.setProperty(LAST_DISPATCH_RESPONSE_PROPERTY, truncateForProperty_(responseBody));

    if (responseCode !== 204) {
      throw new Error(
        'GitHub repository_dispatch failed with HTTP ' +
          responseCode +
          ': ' +
          truncateForProperty_(responseBody)
      );
    }

    clearAccreditationReviewDirty_();
  } finally {
    lock.releaseLock();
  }
}

function markAccreditationReviewDirty_(reason) {
  const properties = PropertiesService.getScriptProperties();
  properties.setProperty(DIRTY_FLAG_PROPERTY, 'true');
  properties.setProperty(DIRTY_AT_PROPERTY, new Date().toISOString());
  properties.setProperty(DIRTY_REASON_PROPERTY, reason || 'sheet edited');
}

function clearAccreditationReviewDirty_() {
  const properties = PropertiesService.getScriptProperties();
  properties.deleteProperty(DIRTY_FLAG_PROPERTY);
  properties.deleteProperty(DIRTY_AT_PROPERTY);
  properties.deleteProperty(DIRTY_REASON_PROPERTY);
}

function getAccreditationReviewDispatchStatus() {
  const properties = PropertiesService.getScriptProperties();
  return {
    dirty: properties.getProperty(DIRTY_FLAG_PROPERTY) || '',
    dirtyAt: properties.getProperty(DIRTY_AT_PROPERTY) || '',
    dirtyReason: properties.getProperty(DIRTY_REASON_PROPERTY) || '',
    lastDispatchAt: properties.getProperty(LAST_DISPATCH_AT_PROPERTY) || '',
    lastDispatchStatus: properties.getProperty(LAST_DISPATCH_STATUS_PROPERTY) || '',
    lastDispatchResponse: properties.getProperty(LAST_DISPATCH_RESPONSE_PROPERTY) || ''
  };
}

function getHeaderMap_(sheet) {
  const headers = sheet.getRange(1, 1, 1, sheet.getLastColumn()).getDisplayValues()[0];
  const headerMap = {};
  headers.forEach(function(header, index) {
    const normalized = normalizeCellValue_(header);
    if (normalized) {
      headerMap[normalized] = index + 1;
    }
  });

  ['action_id', 'review_status'].forEach(function(requiredHeader) {
    if (!headerMap[requiredHeader]) {
      throw new Error('Missing required sheet header: ' + requiredHeader);
    }
  });

  return headerMap;
}

function getTouchedHeaders_(range, headerMap) {
  const startColumn = range.getColumn();
  const endColumn = startColumn + range.getNumColumns() - 1;
  return Object.keys(headerMap).filter(function(header) {
    const column = headerMap[header];
    return column >= startColumn && column <= endColumn;
  });
}

function getRowValueByHeader_(rowValues, headerMap, headerName) {
  const columnIndex = headerMap[headerName];
  if (!columnIndex) {
    return '';
  }
  return rowValues[columnIndex - 1];
}

function getSettings_() {
  const properties = PropertiesService.getScriptProperties();
  const githubOwner = cleanPropertyValue_(properties.getProperty('GITHUB_OWNER'));
  const githubRepo = cleanPropertyValue_(properties.getProperty('GITHUB_REPO'));
  const githubToken = cleanPropertyValue_(properties.getProperty('GITHUB_TOKEN'));
  const sheetTabName =
    cleanPropertyValue_(properties.getProperty('REVIEW_SHEET_TAB')) ||
    DEFAULT_SETTINGS.sheetTabName;
  const eventType =
    cleanPropertyValue_(properties.getProperty('DISPATCH_EVENT_TYPE')) ||
    DEFAULT_SETTINGS.eventType;
  const dispatchIntervalMinutes = Number(
    cleanPropertyValue_(properties.getProperty('DISPATCH_INTERVAL_MINUTES')) ||
      DEFAULT_SETTINGS.dispatchIntervalMinutes
  );

  if (!githubOwner || !githubRepo || !githubToken) {
    throw new Error(
      'Missing required Script Properties. Set GITHUB_OWNER, GITHUB_REPO, and GITHUB_TOKEN before enabling triggers.'
    );
  }

  return {
    githubOwner: githubOwner,
    githubRepo: githubRepo,
    githubToken: githubToken,
    sheetTabName: sheetTabName,
    eventType: eventType,
    dispatchIntervalMinutes: dispatchIntervalMinutes
  };
}

function normalizeDispatchInterval_(value) {
  const allowed = [1, 5, 10, 15, 30];
  const interval = Number(value);
  if (allowed.indexOf(interval) === -1) {
    throw new Error(
      'DISPATCH_INTERVAL_MINUTES must be one of: ' + allowed.join(', ')
    );
  }
  return interval;
}

function normalizeCellValue_(value) {
  return String(value || '').trim().toLowerCase();
}

function cleanPropertyValue_(value) {
  return String(value || '').trim();
}

function truncateForProperty_(value) {
  const text = String(value || '');
  return text.length > 8000 ? text.slice(0, 8000) : text;
}
