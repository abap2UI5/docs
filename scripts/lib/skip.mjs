/*
 * skip — what a gate does when the network kept it from its source.
 *
 * On a laptop without a network a gate that cannot read `z2ui5_if_client`,
 * the custom controls or the release API has checked nothing, says SKIPPED
 * and passes: that is not a documentation defect, and a contributor on a
 * train should still be able to run `npm run check`.
 *
 * Under CI it is the other way round. check.yml is what a pull request has
 * to pass and deploy.yml is what stands in front of the published site; a
 * SKIPPED there printed one line in a log nobody reads and turned the run
 * green, which is a gate that can be walked past by an outage. So when `CI`
 * is set (GitHub Actions sets it on every run) a skip is a failure - re-run
 * the job once the source is back.
 */

/** Whether a skipped gate fails this run. */
export const strict = (env = process.env) => Boolean(env.CI);

/** Report a gate that verified nothing, and end the process: 0 on a
 *  laptop, 1 under CI. */
export function skip(what = 'nothing was verified.', { env = process.env, exit = process.exit } = {}) {
  if (strict(env)) {
    console.error(`SKIPPED: ${what}`);
    console.error('Under CI a gate that could not reach its source fails rather than pass having checked nothing - re-run once the source is reachable.');
    return exit(1);
  }
  console.log(`SKIPPED: ${what}`);
  return exit(0);
}
