# Zenodo submission email

New proposals can email a short summary to the maintainer. The message links to
the matching pending proposal in the local reviewer app. Only the explicit
**Approve and add to catalogue** action imports its validated index. Opening a
link, an email preview or an automated mail scanner cannot approve anything.
Replies to email are not processed. No point clouds are copied.

Set this administrator option before launching the submission app:

```r
options(alsdownloader.submission_mail = list(
  from = "notifications@example.org", # verified sender at your mail provider
  to = "maintainer@example.org",
  smtp_server = "smtps://smtp.example.org:465",
  review_url = "http://127.0.0.1:8792/",
  preview = TRUE
))
```

With `preview = TRUE`, submitting saves a private `.eml` preview under
`submission_dir/notifications/`; no message is sent. For real delivery, configure
`ALS_SMTP_USERNAME` and `ALS_SMTP_PASSWORD` in the host's private environment,
then set `preview = FALSE`. Do not commit credentials or put them in proposals.
SMTP uses required TLS through [curl::send_mail](https://jeroen.r-universe.dev/curl/doc/manual.html#send_mail).

The recipient and sender come only from administrator configuration. The
contributor's optional address appears in the message body, never as a recipient
or reply-to header. Messages include title, DOI, acquisition, platform, asset
count and review link. These messages and previews contain private contacts;
restrict the queue directory to the operator and apply the same retention policy
as proposals. Disable notifications by setting the option to `NULL`.

Delivery status is stored in `notifications/<id>.json`: `preview`, `sent` or
`failed`. `sent` means SMTP accepted the message, not proof of inbox delivery.
An identical submission does not resend after recorded success. Failed messages
can be retried by calling `submit_zenodo()` with the saved proposal and same queue.
Submissions remain pending even if email fails; inspect the private queue as well.
A process failure after SMTP accepts but before saving its receipt may cause a
duplicate on retry. Sending may take up to 20 seconds; this is not a bulk mail
service or a tested 100-submissions-per-day deployment.

The review link currently works on the maintainer's PC while its reviewer app is
running. The submission service and reviewer must use the same private queue.
Do not expose the local review panel publicly. Reviewing from a phone or another
computer needs a separately deployed, authenticated reviewer service; this
implementation does not provide approval inside a mail client or public approval
tokens.
