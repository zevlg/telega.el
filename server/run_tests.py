#!/usr/bin/env python3
import sys
import os
import os.path
import subprocess


TELEGA_SERVER = os.path.expanduser('~/.telega/telega-server')

# Tuple - (bool, json, plist).  If bool is True then test must pass,
# if false then it must fail
TESTS = [
    (True, '{"hello":30}', '(:hello 30)'),
    (True, '[true]', '[t]'),
    (True, '{"@type":"sendMessage","chat_id":10,"input_message_content":{"@type":"inputMessageText","text":{"@type":"formattedText","text":"test"}}}', '(:@type "sendMessage" :chat_id 10 :input_message_content (:@type "inputMessageText" :text (:@type "formattedText" :text "test")))'),

    # space is not allowed
    (False, '{"hello there":40}', '(:hello there 40)'),

    # TODO: add test cases here
]


def run_telega(input, flag):
    """Return tuple - (status, output)."""
    proc = subprocess.Popen([TELEGA_SERVER, flag], stdin=subprocess.PIPE,
                            stdout=subprocess.PIPE)
    output = proc.communicate(input.encode(), timeout=1)[0]
    return proc.returncode, output.rstrip().decode()


def run_single_test(test, flag):
    """Return True if TEST passed.
    FLAG is passed to telega-server."""
    try:
        status, output = run_telega(test[1], flag)
    except subprocess.TimeoutExpired:
        status = -1
    except OSError as e:
        raise e

    if test[0] and status != 0:
        print('FAILED  %s -> %s' % (test[1], test[2]))
        return False
    elif test[0] and test[2] != output:
        print('FAILED  %s -> %s   UNEXPECTED result: %s' % (
            test[1], test[2], output))
        return False
    else:
        print('PASSED  %s -> %s' % (test[1], test[2]))
        return True


def run_tests(args):
    results = []

    print("JSON --> PLIST:")
    for test in TESTS:
        results.append(run_single_test(test, '-j'))

    print()
    print("PLIST --> JSON:")
    for test in TESTS:
        results.append(run_single_test((test[0], test[2], test[1]), '-p'))

    print()
    print("Ran %d tests, %d passed, %d failed" % (
        len(results), results.count(True), results.count(False)))
    print()

    if results.count(False) > 0:
        sys.exit(-1)


if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser()
    args = parser.parse_args()

    run_tests(args)
