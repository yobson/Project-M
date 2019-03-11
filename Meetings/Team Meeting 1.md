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

1. Get tasks list
2. Run task
3. Schedule the nieve way

### After that:

1. JS library with phone censors
2. Track 'wealth'
3. Get\register ID
4. Better scheduler (when screen off etc)

### After $ after that:

1. API for getting 'wealth'
2. More security!!!
3. Make it look pretty

