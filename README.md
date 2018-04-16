# logdog

Just like docker compose but with normal programs.

Aggregates logs nicely.

Demo: <https://asciinema.org/a/vJMpIfqfVAxHu8iWdRkNYcXW7>

## Operation

Two modes of operation:

* Shell Arguments
* Configuration

### Shell Arguments

If you don't supply any config input, then the arguments are interpreted as shell strings.

These will be named "process_N" sequentially and run until completion or failure.

### Configuration

If you supply configuration input, then your process list will be defined in the config file format.

Any arguments provided will be used to filter the process list.

#### Config Example

```
---
osname:
  process: uname
  args:
    - "-a"

echo:
  shell: "echo bar && sleep 1 && exit 1"
  resume:
    - fail

testscript:
  process: ./test/test.bash
  resume:
    - succeed
    - fail
```

Output:

```
$ logdog < test/processes.yaml
testscript | Starting Process P {name = "testscript", runner = Program "./test/test.bash" [], resumption = Resume {succeed = True, failure = True}}
osname     | Starting Process P {name = "osname", runner = Program "uname" ["-a"], resumption = Resume {succeed = False, failure = False}}
echo       | Starting Process {"name":"echo","resumption":{"succeed":false,"failure":true},"runner":{"tag":"Shell","contents":"echo bar && sleep 1 && exit 1"}}
osname     | stdout -> Darwin host.local xx.x.x Darwin Kernel Version xx.x.x: Thu 2018; ...
echo       | stdout -> bar
osname     | Exited Successfully
testscript | stdout -> test.bash 1
testscript | stdout -> test.bash 2
testscript | stdout -> test.bash 3
testscript | stdout -> test.bash 4
echo       | Failure -> Failed with code 1
echo       | Restarting process after failure with exit code 1
echo       | stdout -> bar
testscript | stdout -> test.bash 5
testscript | stdout -> test.bash 6
testscript | stdout -> test.bash 7
echo       | Failure -> Failed with code 1
echo       | Restarting process after failure with exit code 1
echo       | stdout -> bar
testscript | stdout -> test.bash 8
testscript | stdout -> test.bash 9
^C
```


## Bugs

* Doesn't quit in GHCi when using Ctrl-C


## TODO

* Color Lines
