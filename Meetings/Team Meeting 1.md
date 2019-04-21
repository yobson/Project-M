# Team Meeting 1

## Holistic Idea

- Server side:
  - Identify phones
  - Parameterize JS
  - Deal with results
  - Display projects
- Phone side
  - Select project
  - Schedule runs (in background)
  - run in background
- JS
  - Write library to get data and send results

### Running order

#### First run

1. Phone Requests an ID or registers ID
2. Phone Given list of projects
3. Phone selects project(s)
4. phone schedules what to run when

#### When running

1. When scheduled, request project page in background
2. Server gets requests, possibly customises response, sends JS back
3. phone runs JS (this is done automatically me thinks)
4. JS library sends results back to server (AJAX Jobby)
5. Server updates users 'wealth'

## Roadmap

### Core tasks:

- [x] Get tasks list
- [x] Run task
- [x] Schedule the nieve way

### Tasks we would like:

- [x] Track 'wealth'
- [x] Get\register ID
- [x] Better scheduler (when screen off etc)

### Tasks we really don't need to do... but want to do:

- [x] API for getting 'wealth'
- [ ] JS library with phone censors (Might add for demo)
- [ ] More security!!!
- [ ] Make it look pretty

