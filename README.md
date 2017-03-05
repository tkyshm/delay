delay
=====

An OTP application

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
webhook    | executes to get webhook when a job is finished. (optional)
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

### TODO:
- [x] enqueue
- [x] docker
- [x] mnesia
- [x] job worker
- [x] dequeue
- [ ] test code
- [ ] ci
- [ ] load experience
