// Build and test using setup-ocaml@v2 mostly.

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
    //'fail-fast': false,
    matrix: {
      os: [
        'ubuntu-latest',
        //TODO: 'macos-latest'
        //TODO: 'windows-latest'
      ],
      'ocaml-compiler': [
        '4.14.1',
        '5.2.0',
      ],
    },
  },
  'runs-on': '${{ matrix.os }}',
  steps: [
    checkout,
    {
      uses: 'ocaml/setup-ocaml@v2',
      with: {
        'ocaml-compiler': '${{ matrix.ocaml-compiler }}',
        //'opam-depext': false,
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
      run: 'make',
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
