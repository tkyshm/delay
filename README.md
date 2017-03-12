[![Build Status](https://travis-ci.org/tkyshm/delay.svg?branch=master)](https://travis-ci.org/tkyshm/delay)

delay
=====

Simple delay job Application

Build
-----

    $ rebar3 compile

----

## API
### POST /api/enqueue
- Content-Type: application/json
- Post parameters

param      | description
---------- | -------------------------------------------------------
exec\_time | exec\_time is specified by unixtime (seconds). (optional)
webhook    | GET request to webhook when a job is finished. (optional)
data       | user job's parameters. (optional)

```json
{
    "event": "event_name",
    "exec_time": 1488714710,
    "webhook": "hook_url",
    "data" : {
        "id": "xxx",
        "name": "kirito"
    }
}
```

### GET /api/dequeue

- Support long-polling
- Request headers:
header          | value
--------------- | --------------------------------------
X-Delay-Timeout | Long-polling timeout (seconds).

- Response:
```json
[
  {
    "uid": "afcc27ac-3f03-4cdd-ad73-ddd9422b56d6",
    "data": {
      "name": "abc",
      "id": "12304"
    }
  },
  {
    "uid": "ad09e2c3-f14f-4c51-a8df-6b7a3b7ba3d6",
    "data": {
      "name": "defg",
      "id": "348593"
    }
  }
]
```

### TODO:
- [x] enqueue
- [x] docker
- [x] mnesia
- [x] job worker
- [x] dequeue
- [x] long-polling
- [x] webhook
- [x] test code
- [x] ci
- [ ] load experience
- [ ] document
- [ ] cleans commit
