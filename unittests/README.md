To run the tests:
  1. `npm install` to get Node.js dependencies
  2. `./start_conflux.sh` in a separate terminal, and start Ganache (for Ethereum tests)
    -  Be sure to clone the `conflux-rust` repo, and within `start_conflux.sh`, set `conflux` to the location of your repo clone.
    -  Ganache can be run with `npm run ganache` if `npm install` was used to install dependencies.
  3. `./run_unittests.sh`
