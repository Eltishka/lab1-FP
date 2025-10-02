def largest_product_in_series(digits):
    max_product = 0
    for i in range(len(digits) - 12):
        product = 1
        for j in range(13):
            product *= int(digits[i + j])
        max_product = max(max_product, product)
    
    return max_product