# Contributor contact and approval

The app requires a contact email to prepare a private submission. It does not publish that address in a GitHub issue, send email automatically, or store requests in a database. Form values remain in the Shiny session; the connection worker writes temporary progress text that is removed after completion or cancellation. The external data host sees the connection request but is not sent the contact email as a separate field.

**Send my request** opens the user's mail client. The user reviews and sends the draft to Cesar Alvites. The maintainer can reply using that email thread after review and integration. Email handling then follows the sender's and recipient's mail-service policies. There is no automated mail service configured.

Suggested acceptance reply (send only after integration):

> Subject: Your dataset has been added to ALS Downloader
>
> Thank you for contributing [dataset name, DOI]. Your source is now included in ALS Downloader: [app or catalog link]. Data remain hosted by the original provider with the stated license and attribution. Please reply to this email if the access link or metadata change.
>
> Cesar Alvites, developer and maintainer

For approval-time statistics, create a metadata-only GitHub issue with the title `Dataset suggestion: ...`; do not copy the contributor's email. Apply `source-approved` only after acceptance. Email-only requests are excluded from GitHub statistics, so these figures do not represent every request.
