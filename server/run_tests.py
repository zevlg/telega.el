#!/usr/bin/env python3
import sys
import os
import os.path
import subprocess


TELEGA_SERVER = os.path.expanduser('~/.telega/telega-server')
if not os.path.isfile(TELEGA_SERVER):
    TELEGA_SERVER += '.exe'

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

PLIST_TO_JSON_TESTS = TESTS

JSON_TO_PLIST_TESTS = TESTS + [
    # emojis
    (True, b'{"hello":"hello \\u231A there and \\u231A"}',
     '(:hello #("hello \\u231A there and \\u231A" 6 7 (telega-emoji-p t telega-display "\\u231A") 18 19 (telega-emoji-p t telega-display "\\u231A")))'),
    (True, b'{"hello":"joy \ud83d\ude02 here lala"}',
     '(:hello #("joy \\ud83d\\ude02 here lala" 4 6 (telega-emoji-p t telega-display "\\U0001f602")))'),

    (True, b'{"ana": "\u043e?\n\n\u0418\u043b\u043b\u044e\u0437\u0438\u044f \u041e\u0431\u043c\u0430\u043d\u0430\ud83c\udfb2"}',
     '(:ana #("\\u043e?\n\n\\u0418\\u043b\\u043b\\u044e\\u0437\\u0438\\u044f \\u041e\\u0431\\u043c\\u0430\\u043d\\u0430\\ud83c\\udfb2" 18 20 (telega-emoji-p t telega-display "\\U0001f3b2")))'),
    # flags
    (True, b'{"flags": "flags \\ud83c\\uddf7\\ud83c\\uddfa"}',
     '(:flags #("flags \\ud83c\\uddf7\\ud83c\\uddfa" 6 10 (telega-emoji-p t telega-display "\\U0001f1f7\\U0001f1fa")))'),
    # city
    (True, b'{"city": "\ud83c\udf0f"}',
     '(:city #("\\ud83c\\udf0f" 0 2 (telega-emoji-p t telega-display "\\U0001f30f")))'),
    # Last char begins emoji sequence
    (True, b'{"@type":"formattedText","text":"\u042f, \u043a\u0441\u0442\u0430\u0442\u0438, \u0442\u043e\u0436\u0435, \u0438 \u0434\u0430\u0436\u0435 \u043d\u0435 \u0437\u0430\u0434\u0443\u043c\u044b\u0432\u0430\u043b\u0441\u044f \ud83e\udd37\u200d\u2642","entities":[]}',
     '(:@type "formattedText" :text #("\\u042f, \\u043a\\u0441\\u0442\\u0430\\u0442\\u0438, \\u0442\\u043e\\u0436\\u0435, \\u0438 \\u0434\\u0430\\u0436\\u0435 \\u043d\\u0435 \\u0437\\u0430\\u0434\\u0443\\u043c\\u044b\\u0432\\u0430\\u043b\\u0441\\u044f \\ud83e\\udd37\\u200d\\u2642" 39 41 (telega-emoji-p t telega-display "\\U0001f937")) :entities [])'),

    # joystick
    (True, b'"\ud83d\udd79"', '#("\\ud83d\\udd79" 0 2 (telega-emoji-p t telega-display "\\U0001f579"))'),

    # Lightning, \u26a1 in 13.0 and \u26a1\ufe0f in 12.3
    (True, b'"\u26a1\ufe0f"', '#("\\u26a1\\ufe0f" 0 2 (telega-emoji-p t telega-display "\\u26a1\\ufe0f"))'),

    # See https://github.com/zevlg/telega.el/issues/251
    (True, b'"\ud835\udd98\ud835\udd94\ud835\udd92\ud835\udd8a\ud835\udd99\ud835\udd8d\ud835\udd8e\ud835\udd93\ud835\udd8c here is new"', '#("\\ud835\\udd98\\ud835\\udd94\\ud835\\udd92\\ud835\\udd8a\\ud835\\udd99\\ud835\\udd8d\\ud835\\udd8e\\ud835\\udd93\\ud835\\udd8c here is new" 0 2 (telega-display "\\U0001d598") 2 4 (telega-display "\\U0001d594") 4 6 (telega-display "\\U0001d592") 6 8 (telega-display "\\U0001d58a") 8 10 (telega-display "\\U0001d599") 10 12 (telega-display "\\U0001d58d") 12 14 (telega-display "\\U0001d58e") 14 16 (telega-display "\\U0001d593") 16 18 (telega-display "\\U0001d58c"))'),

    # red heart, see https://t.me/tdlibchat/42441
    (True, b'"\u2764\ufe0f"', '#("\\u2764\\ufe0f" 0 2 (telega-emoji-p t telega-display "\\u2764\\ufe0f"))'),
    (True, b'"\u2764"', '#("\\u2764" 0 1 (telega-emoji-p t telega-display "\\u2764"))'),

    # Heat on fire
    (True, b'"hello there \u2764\ufe0f\u200d\ud83d\udd25"', '#("hello there \\u2764\\ufe0f\\u200d\\ud83d\\udd25" 12 17 (telega-emoji-p t telega-display "\\u2764\\ufe0f\\u200d\\U0001f525"))'),

    # Treat <digit>⃣ as emoji as in official client
    (True, b'"3\u20e3\u20e30\u20e30\u20e30\u20e3"', '#("3\\u20e3\\u20e30\\u20e30\\u20e30\\u20e3" 0 2 (telega-emoji-p t telega-display "3\\u20e3") 3 5 (telega-emoji-p t telega-display "0\\u20e3") 5 7 (telega-emoji-p t telega-display "0\\u20e3") 7 9 (telega-emoji-p t telega-display "0\\u20e3"))')

    # TODO: [wo]man shrug without trailing \ufe0f should be an emoji, as in
    # official client
    # (True, b'"\ud83e\udd37\u200d\u2642"', '#("\\u2764" 0 4 (telega-emoji-p t telega-display "\\U0001f937\\u200d\\u2642"))'),
]

def run_telega(input, flag):
    """Return tuple - (status, output)."""
    proc = subprocess.Popen([TELEGA_SERVER, flag], stdin=subprocess.PIPE,
                            stdout=subprocess.PIPE)
    if not isinstance(input, bytes):
        input = input.encode()
    output = proc.communicate(input, timeout=1)[0]
    return proc.returncode, output.rstrip().decode()


def run_single_test(test, flag):
    """Return True if TEST passed.
    FLAG is passed to telega-server."""
    should_pass, input, expected = test
    try:
        status, output = run_telega(test[1], flag)
    except subprocess.TimeoutExpired:
        status = -1
    except OSError as e:
        raise e

    if should_pass:
        if status != 0:
            print('FAILED  %s   with status=%d, but was expecting success' % (
                input, status))
            return False
        elif expected != output:
            print('FAILED  %s -> %s   but was expecting: %s' % (
                input, output, expected))
            return False
        else:
            print('PASSED  %s -> %s' % (input, output))
            return True
    else:
        # should fail
        if status == 0:
            print('FAILED  %s -> %s   parsed, while expecting it to fail' % (
                input, output))
            return False
        else:
            print('PASSED  %s  failed to parse, as expected' % (input, ))
            return True


def run_tests(args):
    results = []

    print("JSON --> PLIST:")
    for test in JSON_TO_PLIST_TESTS:
        results.append(run_single_test(test, '-j'))

    print()
    print("PLIST --> JSON:")
    for test in PLIST_TO_JSON_TESTS:
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
