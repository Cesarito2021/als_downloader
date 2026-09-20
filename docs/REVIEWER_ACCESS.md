# Administrator access

Contributors submit without signing in. Opening **Review submissions** requires
an administrator email identifier and an ALS Downloader password. The identifiers
are configured by the host owner; entering someone else's email is not sufficient.
This is not Gmail/Outlook sign-in or proof of mailbox ownership. No email is sent.

## Private setup

Install optional `sodium` and `askpass` dependencies. On the administrator's PC:

```r
alsdownloader::configure_reviewer_access(
  path = "PRIVATE_DIRECTORY/reviewer.rds",
  emails = c("owner@example.org", "backup@example.org")
)
```

Two native password prompts request a new app password of 15–256 characters.
Use a dedicated passphrase, not an email password. All configured identifiers
belong to the same administrator account and share this password. Only its salted
scrypt hash is stored. Cancellation or mismatched entries do not change access.
There is no default password, registration form or email-based password reset.

Keep the file outside Git, web assets and the submission directory, in a directory
restricted to the service owner. POSIX file permissions are requested by the setup
function; on Windows configure the parent directory's ACL separately. A user who
controls the host filesystem can change credentials and queue files; this login
is not protection against a compromised host or OS administrator.

Before launching, set the server-side option:

```r
options(alsdownloader.reviewer_credentials = "PRIVATE_DIRECTORY/reviewer.rds")
alsdownloader::launch_app(
  submission_dir = "PRIVATE_QUEUE", reviewer = "Maintainer"
)
```

Alternatively set `ALS_REVIEWER_CREDENTIALS` in the host environment. Missing,
invalid or unavailable credentials deny reviewer access. Public submission remains
available. Do not expose configuration or allow contributors to choose this path.

## Review and session protection

After login, inspect the proposal, map, file mapping, dates and terms. Approval
requires the explicit verification checkbox. Rejection records the decision;
closing the panel leaves the request pending. Decisions record the authenticated
identifier as well as the maintainer label. Private contacts and decision notes
remain outside public indexes.

The server checks authorization before reading private proposals or approving,
rejecting and refreshing the queue. A browser-side input or hidden-button change
cannot authorize those actions. Sessions expire 30 minutes after login; **Sign out**
clears access and reloads the page. Replacing the credential file revokes existing
sessions. Five failed attempts pause login for one minute across sessions in the
same app process. This is not a distributed multi-server rate limiter.

## Local and hosted use

The reviewer remains local-mode and localhost-only through `launch_app()`. A
public app must omit `reviewer`. The maintainer's trusted review process and the
public submission process need access to the same persistent private queue.
Do not deploy `als_app(reviewer=...)` directly to a public web server: this local
password feature does not provide HTTPS, identity-provider MFA, a public review
service, or hardened network deployment. Phone access requires a separately
configured authenticated HTTPS deployment.

Trusted R functions such as `review_zenodo_submission()` remain host-administrator
APIs. Never expose them as unauthenticated remote endpoints.
