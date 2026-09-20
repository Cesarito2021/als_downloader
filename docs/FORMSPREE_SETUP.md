# Optional Formspree submission transport

Status: the free community form is configured. A single labelled test from
localhost was accepted by Formspree on 20 September 2026; the maintainer
confirmed receipt in Gmail. This is a proposal inbox, not an automatic approval
service. Hosted deployment still requires its own browser test.

The contributor validates a proposal in the local or hosted Shiny app, then
submits it directly from the browser to Formspree. The app displays confirmation
or errors; an anti-spam challenge can be continued in a provider tab. Contributors do not need a Formspree
account. The maintainer creates a form once and verifies the notification email.
Gmail OAuth and SMTP credentials are not needed for this transport.

## Configuration

The community endpoint is built in for ordinary launches without a local review
queue. To override it, set the public form endpoint before launching the app:

```r
options(alsdownloader.formspree = list(
  endpoint = "https://formspree.io/f/YOUR_FORM_ID",
  attachments = FALSE
))
alsdownloader::launch_app()
```

Alternatively set `ALS_FORMSPREE_ENDPOINT` (links only). Set
`options(alsdownloader.formspree = FALSE)` to disable remote submission. A local
`submission_dir` retains the private queue unless Formspree is explicitly
configured. The endpoint is public; never distribute management API keys.
An explicit Formspree configuration takes precedence over the local queue button. An invalid configuration
shows an error rather than silently falling back to another recipient.

Keep anti-spam protection enabled. A restriction to one website domain can block
local applications: verify both localhost and the deployed app during activation.
Submission is an explicit browser POST; no mail is sent when opening or validating
the form. The app never interprets opening the provider tab as successful delivery.
Repeated clicks are blocked until the contributor explicitly enables a retry.

## Coverage and plans

With attachments disabled, submissions include the version DOI, exact Zenodo
boundary link, file mapping, acquisition dates, platform and optional contact.
Declared approximate squares include the centre, centre-to-side distance in metres
and selected filenames. Uploaded local polygons cannot be silently omitted: the
form asks for a boundary already in Zenodo or for attachment support.

Enable `attachments = TRUE` only after enabling file uploads in the Formspree
plan. The complete validated proposal (including geometry) is attached as JSON,
bounded to 8 MiB. Point clouds are never uploaded to Formspree. The browser must
support File and DataTransfer; preparation failure prevents submission.

At review time, verify the current [account limits](https://help.formspree.io/articles/account-management/account-limits)
and [file-upload terms](https://help.formspree.io/articles/building-your-form/file-uploads).
The free plan currently allows 50 submissions/month and 30 days of history;
native file uploads require a paid plan. These are service quotas, not Gmail
storage limits. Formspree receives the submitted metadata and optional contact;
the interface discloses this before sending.

A confirmed monthly-quota error displays: "The monthly limit of 50 submissions
has been reached. Please try again after the monthly quota resets. Save your
proposal to keep a copy." The documented service can also return HTTP 429 for a
short-term rate limit, so 429 alone does not assert monthly exhaustion. Network,
validation and unknown responses use separate messages. The exact reset date is
not promised: it must be confirmed for the active account. No automatic retries
consume quota. The 50-submission wording must be updated if the plan changes.

## CRAN considerations

This optional feature does not require an account to install, load, test or use
the package's other functions. No requests run during installation or tests;
only an explicit Submit click sends data. Browser code is bundled, with no CDN
dependency or vendored Formspree SDK. No credentials are distributed. Failures
leave the proposal available to save, and tests simulate service responses.
This follows the [CRAN policy on unavailable Internet resources](https://cran.r-project.org/web/packages/policies.html);
it is not a guarantee of CRAN acceptance. The public endpoint passed a local live test. Hosted deployment and hosted
approval remain separate release requirements.

## Review and publication

A Formspree receipt is not a trusted approval, a Git commit, or a signed reviewer
invitation. No approval secret is generated in the contributor's app. Received
links must be inspected and used to rebuild a proposal; attached proposals are
untrusted and require the same metadata, geometry and licence checks as other
submissions. The existing maintainer R review functions can publish a verified
proposal into a private local queue. Hosted email approval and central catalogue
publication (including a possible GitHub commit) remain separate deployment work.
There is no automatic acceptance, deadline, or public exposure of contact details.

For Gmail organization, notifications use the subject prefix `[ALS Downloader]`.
After a live message arrives, verify its sender and subject before creating a
Gmail filter that applies the `ALS Downloader` label. No Gmail rules are changed
by this package.

## Activation checklist

1. Maintainer creates and verifies a form with the intended recipient.
2. Configure its endpoint and chosen attachment capability.
3. Send one agreed test from localhost and one from the hosted app; confirm
   provider success and actual mailbox delivery, including anti-spam behaviour.
4. Check that boundary links, mapping and optional contact arrive intact; test a
   polygon attachment if enabled. Do not approve the test dataset automatically.
5. Configure the mailbox label/filter after confirming the actual email format.

Automated tests use synthetic proposals and intercepted browser POSTs. They do
not constitute a live Formspree or email-delivery test.

## Live test evidence

The received test used DOI `10.5281/zenodo.20311343`, boundary
`5_Marsh_area.zip`, mapped archive `2_Extracted_trees_shrubs_points.zip`, blank
contact and acquisition date, and an explicit TEST ONLY status. Formspree showed
one received submission, no spam, and the intact mapping. The maintainer confirmed
mailbox receipt in the task. No data was approved or committed to the catalogue.
