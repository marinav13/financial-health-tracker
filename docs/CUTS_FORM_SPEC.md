# Cuts Submission Form — build spec

Ready-to-build spec for the Hechinger-owned Google Form that replaces the
college-cuts.com submission link in `cuts.html`. Build at forms.google.com
with a Hechinger Google account (so responses land in a Hechinger-owned
response sheet), then give Claude the form's share URL to swap into the page.

The category options mirror the tracker's `cut_type` vocabulary so
submissions map cleanly onto the pipeline.

---

**Form title:** Report a college cut

**Description:**
The Hechinger Report tracks staffing and program cuts at U.S. four-year
colleges for the College Financial Health Tracker
(financialtracker.hechingerreport.org). If you know about a cut we're
missing, tell us below. We verify every submission before publishing —
nothing appears automatically. Fields marked * are required.

---

1. **College or university name*** — Short answer

2. **State*** — Dropdown (the 50 states + District of Columbia + Puerto Rico)

3. **What kind of cut is it?*** — Multiple choice:
   - Staff or faculty layoffs
   - Programs suspended or eliminated
   - Department or school closure
   - Campus closure
   - Entire institution closing
   - Hiring freeze or furloughs
   - Other (please describe below)

4. **What happened?*** — Paragraph
   Description: Tell us what was cut, how many people or programs are
   affected if you know, and when it takes effect.

5. **When was it announced?** — Date
   Description: Approximate is fine.

6. **Link to a source** — Short answer
   Description: A news story, official announcement, letter, or meeting
   minutes we can verify. If you don't have a link, describe where the
   information came from.
   (Optional response validation: Text → URL, if a link is required later.)

7. **How do you know about this?** — Multiple choice:
   - I work or worked at this institution
   - I'm a student or family member
   - I saw news coverage
   - Other

8. **Your email (optional)** — Short answer
   Description: Only if you're open to a reporter following up. We won't
   publish your name or contact information.

9. **Anything else we should know?** — Paragraph (optional)

---

**Settings to flip:**
- Responses → link to a spreadsheet (Marina reviews it as part of weekly
  cuts triage)
- Do NOT collect email addresses automatically (question 8 makes it opt-in)
- Do NOT limit to 1 response / require sign-in (sources may not have Google
  accounts and shouldn't have to identify themselves)
- Turn on response notifications (Forms → Responses → ⋮ → email
  notifications) so submissions don't sit unseen between weekly reviews

**After building:** send Claude the form's public URL; the submission links
in `cuts.html` (two spots) get repointed in one commit, ported to the public
repo.
