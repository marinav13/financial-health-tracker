import fs from "node:fs/promises";
import path from "node:path";
import { Workbook, SpreadsheetFile } from "@oai/artifact-tool";

const outputDir = process.cwd();
const dataPath = path.join(outputDir, "grant_witness_tracker_issue_data.json");
const workbookPath = path.join(outputDir, "grant_witness_termination_review.xlsx");
const previewSummaryPath = path.join(outputDir, "summary_preview.png");
const previewIssuesPath = path.join(outputDir, "issues_preview.png");

const payload = JSON.parse(await fs.readFile(dataPath, "utf8"));
const summaryRows = payload.summary || [];
const agencySummary = payload.agency_summary || [];
const institutionSummary = payload.institution_summary || [];
const issueRows = payload.issue_rows || [];
const allRows = payload.all_rows || [];

const workbook = Workbook.create();
workbook.setColorScheme({
  name: "Hechinger Data",
  themeColors: {
    accent1: "#1f4d8f",
    accent2: "#c65f2d",
    accent3: "#7c8f3a",
    accent4: "#cda434",
    accent5: "#6d6a75",
    accent6: "#a1412c",
    dk1: "#111111",
    lt1: "#ffffff",
    lt2: "#f4efe6",
    hlink: "#1f4d8f",
    folHlink: "#6d6a75"
  }
});

const summarySheet = workbook.worksheets.add("Summary");
const institutionsSheet = workbook.worksheets.add("Institution Summary");
const issuesSheet = workbook.worksheets.add("Still Labeled Terminated");
const referenceSheet = workbook.worksheets.add("All Restored Rows");

summarySheet.getRange("A1").values = [[
  "Grant Witness tracker review: grants still labeled Terminated despite restoration evidence"
]];
summarySheet.getRange("A1:F1").format = {
  fill: "accent1",
  font: { color: "lt1", bold: true, size: 16 },
  wrapText: false,
  verticalAlignment: "center"
};
summarySheet.getRange("A1:F1").format.rowHeightPx = 34;

summarySheet.getRange("A2").values = [[
  "Scope: higher-ed grants matched to institutions in our tracker where the joined pipeline classifies the grant as not_currently_disrupted."
]];
summarySheet.getRange("A2:F2").format = {
  fill: { type: "solid", color: { type: "theme", value: "accent1", transform: { lighten: 86 } } },
  font: { color: "dk1", italic: true },
  wrapText: true
};

summarySheet.getRange(`A4:B${summaryRows.length + 3}`).values = summaryRows.map((row) => [row.metric, row.value]);
summarySheet.getRange(`A4:B${summaryRows.length + 3}`).format.borders = {
  preset: "outside",
  style: "thin",
  color: "#d7cdbf"
};
summarySheet.getRange(`A4:A${summaryRows.length + 3}`).format = {
  fill: { type: "solid", color: { type: "theme", value: "accent1", transform: { lighten: 78 } } },
  font: { bold: true, color: "dk1" },
  wrapText: true,
  verticalAlignment: "top"
};
summarySheet.getRange(`B4:B${summaryRows.length + 3}`).format = {
  wrapText: true,
  verticalAlignment: "top"
};

summarySheet.getRange(`D4:F${agencySummary.length + 4}`).values = [
  ["Agency", "All restored-related grants", "Still labeled Terminated"],
  ...agencySummary.map((row) => [row.agency, Number(row.all_issue_grants || 0), Number(row.still_labeled_terminated || 0)])
];
summarySheet.getRange(`D4:F4`).format = {
  fill: "accent2",
  font: { color: "lt1", bold: true },
  wrapText: true
};
summarySheet.getRange(`D4:F${agencySummary.length + 4}`).format.borders = {
  preset: "outside",
  style: "thin",
  color: "#d7cdbf"
};

summarySheet.getRange("D11:G11").values = [["Top institutions", "City", "State", "Still labeled Terminated"]];
summarySheet.getRange("D11:G11").format = {
  fill: "accent3",
  font: { color: "lt1", bold: true }
};
summarySheet.getRange(`D12:G${Math.min(institutionSummary.length + 11, 21)}`).values = institutionSummary
  .slice(0, 10)
  .map((row) => [
    row.institution_name,
    row.city,
    row.state,
    Number(row.still_labeled_terminated || 0)
  ]);
summarySheet.getRange(`D11:G${Math.min(institutionSummary.length + 11, 21)}`).format.borders = {
  preset: "outside",
  style: "thin",
  color: "#d7cdbf"
};
summarySheet.getRange(`D12:G${Math.min(institutionSummary.length + 11, 21)}`).format.wrapText = true;

summarySheet.freezePanes.freezeRows(3);
summarySheet.getRange("A:A").format.columnWidthPx = 270;
summarySheet.getRange("B:B").format.columnWidthPx = 460;
summarySheet.getRange("C:C").format.columnWidthPx = 18;
summarySheet.getRange("D:D").format.columnWidthPx = 210;
summarySheet.getRange("E:E").format.columnWidthPx = 110;
summarySheet.getRange("F:F").format.columnWidthPx = 130;
summarySheet.getRange("G:G").format.columnWidthPx = 145;
summarySheet.getRange("H:H").format.columnWidthPx = 18;

const institutionHeaders = [
  "Institution",
  "City",
  "State",
  "All restored-related grants",
  "Still labeled Terminated",
  "Reference reinstated rows",
  "Agencies"
];

institutionsSheet.getRange("A1:G1").values = [institutionHeaders];
institutionsSheet.getRange(`A2:G${institutionSummary.length + 1}`).values = institutionSummary.map((row) => [
  row.institution_name,
  row.city,
  row.state,
  Number(row.all_issue_grants || 0),
  Number(row.still_labeled_terminated || 0),
  Number(row.reinstated_reference_rows || 0),
  row.agencies
]);
institutionsSheet.getRange(`A1:G1`).format = {
  fill: "accent1",
  font: { color: "lt1", bold: true },
  wrapText: true
};
institutionsSheet.getRange(`A1:G${institutionSummary.length + 1}`).format.borders = {
  preset: "outside",
  style: "thin",
  color: "#d7cdbf"
};
institutionsSheet.getRange(`A2:G${institutionSummary.length + 1}`).format.wrapText = true;
institutionsSheet.freezePanes.freezeRows(1);
institutionsSheet.getRange("A:A").format.columnWidthPx = 220;
institutionsSheet.getRange("B:B").format.columnWidthPx = 110;
institutionsSheet.getRange("C:C").format.columnWidthPx = 120;
institutionsSheet.getRange("D:F").format.columnWidthPx = 110;
institutionsSheet.getRange("G:G").format.columnWidthPx = 130;

const detailHeaders = [
  "Institution",
  "City",
  "State",
  "Agency",
  "Grant ID",
  "Raw status",
  "Termination date",
  "Reinstatement date",
  "Restoration signal",
  "Evidence summary",
  "Project title",
  "Source URL",
  "Detail URL"
];

function fillGrantSheet(sheet, rows) {
  sheet.getRange("A1:M1").values = [detailHeaders];
  sheet.getRange(`A2:M${rows.length + 1}`).values = rows.map((row) => [
    row.institution_name,
    row.city,
    row.state,
    row.agency,
    row.grant_id,
    row.raw_status,
    row.termination_date,
    row.reinstatement_date,
    row.restoration_signal,
    row.evidence_summary,
    row.project_title,
    row.source_url,
    row.detail_url
  ]);
  sheet.getRange("A1:M1").format = {
    fill: "accent1",
    font: { color: "lt1", bold: true },
    wrapText: true
  };
  sheet.getRange(`A1:M${rows.length + 1}`).format.borders = {
    preset: "outside",
    style: "thin",
    color: "#d7cdbf"
  };
  sheet.getRange(`A2:M${rows.length + 1}`).format.wrapText = true;
  sheet.getRange(`A1:M${rows.length + 1}`).format.rowHeightPx = 28;
  sheet.freezePanes.freezeRows(1);
  sheet.freezePanes.freezeColumns(5);
  sheet.getRange("A:A").format.columnWidthPx = 210;
  sheet.getRange("B:B").format.columnWidthPx = 90;
  sheet.getRange("C:C").format.columnWidthPx = 120;
  sheet.getRange("D:D").format.columnWidthPx = 70;
  sheet.getRange("E:E").format.columnWidthPx = 120;
  sheet.getRange("F:F").format.columnWidthPx = 130;
  sheet.getRange("G:H").format.columnWidthPx = 105;
  sheet.getRange("I:I").format.columnWidthPx = 200;
  sheet.getRange("J:J").format.columnWidthPx = 360;
  sheet.getRange("K:K").format.columnWidthPx = 280;
  sheet.getRange("L:M").format.columnWidthPx = 180;
}

fillGrantSheet(issuesSheet, issueRows);
fillGrantSheet(referenceSheet, allRows);

issuesSheet.getRange(`F2:F${issueRows.length + 1}`).format = {
  fill: { type: "solid", color: { type: "theme", value: "accent2", transform: { lighten: 80 } } },
  font: { bold: true, color: "#7a2313" }
};
referenceSheet.getRange(`F2:F${allRows.length + 1}`).conditionalFormats.add("containsText", {
  text: "Reinstated",
  format: { fill: "#e9f5e5", font: { color: "#245b2a", bold: true } }
});
referenceSheet.getRange(`F2:F${allRows.length + 1}`).conditionalFormats.add("containsText", {
  text: "Terminated",
  format: { fill: "#fbe9e3", font: { color: "#7a2313", bold: true } }
});

const summaryPreview = await workbook.render({ sheetName: "Summary", range: "A1:G22", format: "png" });
await fs.writeFile(previewSummaryPath, Buffer.from(await summaryPreview.arrayBuffer()));
const issuesPreview = await workbook.render({ sheetName: "Still Labeled Terminated", range: `A1:M${Math.min(issueRows.length + 1, 8)}`, format: "png" });
await fs.writeFile(previewIssuesPath, Buffer.from(await issuesPreview.arrayBuffer()));

const output = await SpreadsheetFile.exportXlsx(workbook);
await output.save(workbookPath);

console.log(JSON.stringify({
  workbookPath,
  previewSummaryPath,
  previewIssuesPath
}, null, 2));
