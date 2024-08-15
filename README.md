# Structured Eligibility Criteria Proof of Concept (aka SEC POC) Admin App

This is an R/Shiny administration app used in the CTRP SEC POC project.

For development, see the "R Development" section of the [README](https://github.com/CBIIT/sec_poc/blob/master/README.md) for the sec_poc repo.

You will also need to set up your admim credentials in the SQLlite DB by running [init_user_db.R](https://github.com/CBIIT/sec_admin/blob/master/init_user_db.R).

## Production

### Deploying Code to Production

1. Push changes to master branch.
2. Create a PR from master -> prod.
3. Log in to the remote server with your NIH credentials.
4. Authenticate to GitHub with `gh auth login`.
5. Obtain `root` access with `sudo su`.
6. Navigate to the shiny source code folder `/srv/shiny-server/sec-apps/stage/sec_admin`.
7. Make sure the current branch is set to `prod`.
8. Create a commit checkpoint with `git tag save-point`.
9. Run `git pull` to receive the updates from the pull request.
10. If everything looks good, delete the checkpoint tag `git tag -d save-point`. **You're done.**
11. If there's an error with the new changes, you can quickly restore the last working version with `git reset --hard save-point`.
12. Fix the issue, apply the changes to master, repeat the process.
