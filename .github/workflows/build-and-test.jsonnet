// Build and test using setup-ocaml@v3 mostly.

// ----------------------------------------------------------------------------
// Helpers
// ----------------------------------------------------------------------------

local checkout = {
  uses: 'actions/checkout@v3',
};

// ----------------------------------------------------------------------------
// The job
// ----------------------------------------------------------------------------

local job = {
  strategy: {
    matrix: {
      'os': [
        'ubuntu-latest',
        'macos-latest',
	// does not work because of tsdl compilation error on windows
        //'windows-latest'
      ],
      'ocaml-compiler': [
        '4.14.1',
        '5.2.0',
      ],
    },
    //'fail-fast': false,
  },
  'runs-on': '${{ matrix.os }}',
  steps: [
    checkout,
    {
      uses: 'ocaml/setup-ocaml@v3',
      with: {
        'ocaml-compiler': '${{ matrix.ocaml-compiler }}',
        // I used to have "'opam-depext': false" below, but this flag
	// is not supported in setup@v3 and anyway we do want
	// opam-depext otherwise the installation would fail
	// because some packages like ocurl have external
	// dependencies (depext) and require some system packages
	// to be installed. opam-depext does this for us automatically
	// and in a portable way (for linux/mac/windows)!.
      },
    },
    {
      name: 'Install dependencies',
      run: |||
        opam install --deps-only .
      |||,
    },
    {
      name: 'Build',
      run: |||
        eval $(opam env)
        make
      |||,
    },
    {
      name: 'Test',
      run: |||
        eval $(opam env)
        make test
      |||,
    },
    
  ],
};

// ----------------------------------------------------------------------------
// The workflow
// ----------------------------------------------------------------------------
{
  name: 'build-and-test',
  on: {
    // can be run manually from the GHA dashboard
    workflow_dispatch: null,
    // on the PR
    pull_request: null,
    // and another time once the PR is merged on master
    push: {
      branches: [
        'master',
      ],
    },
    // disabled the cron for now because github complains after
    // a few months of inactivity when there is no new commit
    // but the crons ran many times, so simpler to disable for now
    //schedule: [
    //  {
    //    // every day at 12:59
    //    cron: '59 12 * * *',
    //  },
    //],
  },
  jobs: {
    job: job,
  },
}
