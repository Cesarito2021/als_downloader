# Private review from an email invitation

Contributors use **Submit ALS data - Zenodo**, without signing in or opening a
mail client. Their contact email is optional. The ALS Downloader team reviews
submissions; publication always requires an explicit decision. No review deadline is promised.

The configured maintainer receives a summary and a private link. On the PC running
the reviewer app, open that link and click **Open private review**, inspect the
metadata and coverage, then **Approve and add to catalogue** or **Reject**.
There is no app password or separate sign-in code. Opening the email or link does
not approve anything, and the link alone never triggers a decision.

## Host setup

Install `openssl`, configure authenticated [email delivery](ZENODO_EMAIL_SETUP.md),
and create the private recipient allowlist:

```r
alsdownloader::configure_reviewer_access(
  "PRIVATE_DIRECTORY/reviewer.rds", "owner@example.org"
)
options(alsdownloader.reviewer_credentials = "PRIVATE_DIRECTORY/reviewer.rds")
alsdownloader::launch_app(submission_dir = "PRIVATE_QUEUE", reviewer = "Maintainer")
```

The notification recipient must be on this allowlist. Sender credentials belong
to the server configuration, not the contributor or reviewer form. Without a
working mail service proposals remain pending and review access stays locked.
Preview messages cannot authenticate. SMTP acceptance is not proof of inbox delivery.

## Protection and limits

Each invitation contains a random 256-bit secret, bound to one proposal. Only its
hash is stored. A successful delivery receipt is required. Links expire after
30 days and are consumed by the explicit **Open private review** action; review
sessions last 30 minutes. Decisions and allowlist changes revoke access.
The link secret uses a URL fragment and is removed from browser history after
the app receives it. Do not forward invitations. Live messages are not saved to disk.

Private reads and decisions enforce proposal scope on the server. Approval
requires the verification checkbox. A host administrator may reissue an unfinished
invitation by removing its delivery receipt and resubmitting the saved proposal;
this creates a new secret and invalidates the old invitation.

Keep the allowlist and queue outside Git and public assets, accessible only to
the host owner (configure Windows ACLs as appropriate). Host administrators can
modify these files; this feature does not protect a compromised host.

Review remains localhost-only. The app must be running on the maintainer's PC;
phone or remote email review needs a separately deployed HTTPS service. Public
submission and local review must share persistent private storage. Do not deploy
`als_app(reviewer=...)` publicly. Trusted R review functions remain host-owner APIs.
