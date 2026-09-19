# Contributor contact and approval

## Zenodo proposals

The contact email is optional and private. When the administrator configures a
submission directory, clicking **Submit for maintainer review** stores a JSON
proposal there: metadata, coverage polygons, file mapping, acquisition interval,
platform, optional email and submission time. It persists across app sessions.
No point cloud is stored in this queue. Identical metadata/coverage proposals
are deduplicated. A saved proposal JSON also contains the optional email; share
that file privately.

The private reviewer sees the proposal and may approve or reject it. Decisions
retain reviewer identity, time and optional notes. Approved public indexes omit
the contact email and contain dataset metadata, coverage and download links.
If configured, an automatic notification shares the summary and optional contact
with the maintainer through the host's email provider. Zenodo receives metadata or explicitly requested
small coverage-file requests, not the contributor's contact email.

Store the queue outside public web assets with filesystem access restricted to
the service and maintainer. Use persistent storage for hosted submissions and
omit the reviewer option on the public app; use a separate trusted local review
session. Establish retention and deletion handling with contributors before
opening a public service. The current app has no automated retention/deletion
service; maintainers manage those private files directly.

## Other data sources

The separate generic-source form requires a contact email to prepare a private
email draft. **Send my request** opens the user's mail client; the user reviews
and sends it. That form does not itself persist a request or send an email.
Email handling follows the sender's and recipient's mail-service policies.

## Local comparison files

Local LAS/LAZ comparison uploads belong to the Shiny session, not the contribution
queue or catalogue. A hosted instance receives the upload on its server. The
comparison's staged file is deleted on completion, cancellation or session close;
Shiny manages its upload temporary files. Never describe hosted uploads as staying
only on the visitor's computer.

An acknowledgement email may be sent manually after integration, with dataset
name, DOI and catalogue link. Do not publish private contact details in issues.
Historical GitHub issue statistics exclude private queue/email requests and must
not be presented as complete contribution statistics.
# Optional email notifications

When configured by the administrator, a brief proposal summary (including the
optional contributor contact) is sent through the configured SMTP provider to
the maintainer. Private message previews and delivery receipts are stored under
the queue's `notifications` directory and share its access and retention policy.
Opening a notification link never approves a dataset. See
[email setup](ZENODO_EMAIL_SETUP.md) for configuration and limitations.
