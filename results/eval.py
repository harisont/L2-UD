import sys

def read_results(path):
    with open(path) as file:
        return list(map(lambda s: s.strip(), file.readlines()))

def true_positives(actual,expected):
    return [a for a in actual if a in expected]

def false_negatives(actual,expected):
    return [e for e in expected if not e in actual]

def false_positives(actual,expected):
    return [a for a in actual if not a in expected]

if __name__ == "__main__":
    actual = read_results(sys.argv[1])
    expected = read_results(sys.argv[2])
    tp = true_positives(actual,expected)
    fn = false_negatives(actual,expected)
    fp = false_positives(actual,expected)
    tp_n = len(tp)
    fn_n = len(fn)
    fp_n = len(fp)
    print("Precision:", str(round((tp_n / (tp_n + fp_n)) * 100)) + "%")
    print("Recall:   ", str(round((tp_n / (tp_n + fn_n)) * 100)) + "%")
    print()
    print("False negatives:", fn, "({})".format(fn_n))
    print("False positives:", fp, "({})".format(fp_n))
