def get_sum_of_divisors(n):
    s = 1
    for i in range(2, int(n**0.5) + 1):
        if n % i == 0:
            s += i
            if i != n // i:
                s += n // i
    return s

def is_abundant(n):
    return get_sum_of_divisors(n) > n

def sum_of_not_presented_as_two_abundant(limit):
    abundant_numbers = [i for i in range(1, limit + 1) if is_abundant(i)]

    can_be_written = set()
    for i in range(len(abundant_numbers)):
        for j in range(i, len(abundant_numbers)):
            total = abundant_numbers[i] + abundant_numbers[j]
            if total <= limit:
                can_be_written.add(total)
    
    cannot_be_written = [i for i in range(1, limit + 1) if i not in can_be_written]
    
    return sum(cannot_be_written)