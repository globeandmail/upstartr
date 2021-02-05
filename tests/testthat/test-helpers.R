test_that('calc_index indexes correctly', {
  expect_equal(calc_index(c(20, 40, 10)), c(0, 1, -0.5))
  expect_equal(calc_index(c(20, 40, 10), base = 100), c(100, 200, 50))
})

test_that('index correctly uses other bases', {
  expect_equal(calc_index(c(20, 40, 10), base = 100), c(100, 200, 50))
})

test_that('calc_mode returns correct mode', {
  expect_equal(calc_mode(c(1, 1, 1, 2, 2, 3)), 1)
  expect_equal(calc_mode(c('Jane', 'John', 'Stewart', 'Jane', 'Tom')), 'Jane')
  expect_equal(calc_mode(c(TRUE, FALSE, TRUE)), TRUE)
  expect_equal(calc_mode(c(0.1, 0.1, 0.2)), 0.1)
})

test_that('calc_mode returns multiple modes if there are ties', {
  expect_equal(calc_mode(c(1, 1, 1, 2, 2, 2, 3)), c(1, 2))
  expect_equal(calc_mode(c(1, 2)), c(1, 2))
  expect_equal(calc_mode(c('Tom', 'Jane')), c('Tom', 'Jane'))
  expect_equal(calc_mode(c(TRUE, FALSE)), c(TRUE, FALSE))
})

non_utf8 <- 'fa\xE7ile'
Encoding(non_utf8) <- 'latin1'

test_that('unaccent de-accents words', {
  expect_equal(unaccent(non_utf8), 'facile')
  expect_equal(unaccent('Montréal'), 'Montreal')
})

test_that('remove_non_utf8 replaces non-UTF-8 characters with UTF-8 ones', {
  expect_equal(remove_non_utf8(non_utf8), 'façile')
})

test_that('%not_in% returns a logical vector for items from A not in B', {
  expect_equal(c(1, 2, 3, 4, 5) %not_in% c(3, 4, 5, 6, 7), c(TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_equal(c('The', 'quick', 'brown', 'fox') %not_in% c('fox', 'jumped', 'over', 'the', 'lazy', 'dog'), c(TRUE, TRUE, TRUE, FALSE))
})

test_that('not.na returns a logical vector for items that are not NA', {
  expect_equal(not.na(c(1, 2, 3, NA, 4, 5, NA, 6)), c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE))
  expect_equal(not.na(c('Hello', NA, 'World')), c(TRUE, FALSE, TRUE))
  expect_equal(not.na(c(NA, TRUE, FALSE)), c(FALSE, TRUE, TRUE))
})

test_that('simplify_string uppercases', {
  expect_equal(simplify_string('j jonah jameson'), 'J JONAH JAMESON')
})

test_that('simplify_string removes special characters', {
  expect_equal(simplify_string('J. // Jonah Jameson 3'), 'J JONAH JAMESON')
})

test_that('simplify_string removes extra spaces', {
  expect_equal(simplify_string('  J      Jonah Jameson '), 'J JONAH JAMESON')
})

test_that('simplify_string replaces non-UTF-8 characters', {
  expect_equal(simplify_string(non_utf8), 'FACILE')
})

test_that('simplify_string unaccents', {
  expect_equal(simplify_string('Montréal'), 'MONTREAL')
})

test_that('simplify_string removes stopwords', {
  expect_equal(simplify_string(c('123 Canada Inc', 'ABC Ontario Corp.', 'Hello World LLC'), digits = TRUE, stopwords = c('INC', 'CORP', 'LLC')), c('123 CANADA', 'ABC ONTARIO', 'HELLO WORLD'))
})

test_that('simplify_string can handle digits', {
  expect_equal(simplify_string(c('123 Canada Inc.'), digits = TRUE), c('123 CANADA INC'))
})

test_that('simplify_string doesn\'t need to unaccent', {
  expect_equal(simplify_string(c('Montréal'), unaccent = FALSE), c('MONTRAL'))
})

test_that('simplify_string doesn\'t need to remove non-UTF-8 characters', {
  expect_equal(simplify_string(non_utf8, utf8_only = FALSE), c('FACILE'))
})

test_that('simplify_string doesn\'t need to uppercase', {
  expect_equal(simplify_string(c('123 Canada Inc.'), case = 'keep'), c('Canada Inc'))
})

test_that('simplify_string doesn\'t need to trim', {
  expect_equal(simplify_string(c(' Hello   world ! '), trim = FALSE), c(' HELLO   WORLD  '))
})

test_that('simplify_string doesn\'t trip on NAs, NULLs or empty strings', {
  expect_equal(simplify_string(c('Hello World!', NA, 'Test', NULL, '', 'Hi there', character(0))), c('HELLO WORLD', NA, 'TEST', '', 'HI THERE'))
})

test_that('clean_columns forces names into tidyverse style', {
  cols <- c('Date of Purchase', 'Item No.', 'description', 'Transaction at Jane\'s Counter?', 'Auditing - Worth it?')
  expect_equal(clean_columns(cols), c('date_of_purchase', 'item_no', 'description', 'transaction_at_janes_counter', 'auditing_worth_it'))
})

test_that('clean_columns will name empty strings to \'column\'', {
  expect_equal(clean_columns(c('first column', '', 'third column')), c('first_column', 'column_2', 'third_column'))
})

test_that('clean_columns will index columns with the same name', {
  expect_equal(clean_columns(c('Date', 'Purchase Date', 'Date')), c('date', 'purchase_date', 'date_3'))
})

test_that('convert_str_to_logical will convert things to logical', {
  trues <- c('TRUE', 'T', 'True', 'tRUE', 'true', 't', 'Y', 'Yes', 'YES', 'yes', 'y')
  falses <- c('FALSE', 'F', 'False', 'fALSE', 'false', 'f', 'N', 'No', 'NO', 'no', 'n')
  t_len <- length(trues)
  f_len <- length(falses)
  expect_equal(convert_str_to_logical(trues), rep(TRUE, t_len))
  expect_equal(convert_str_to_logical(falses), rep(FALSE, f_len))
  expect_equal(convert_str_to_logical(c(trues, falses)), c(rep(TRUE, t_len), rep(FALSE, f_len)))
})

test_that('convert_str_to_logical can handle custom truthy and falsy values', {
  expect_equal(convert_str_to_logical(c('si', 'no', 's', 'n'), truthy = c('si', 's'), falsy = c('no', 'n')), c(TRUE, FALSE, TRUE, FALSE))
})

test_that('convert_str_to_logical turns non-truthy, non-falsy values into NAs', {
  expect_equal(convert_str_to_logical(c('Hello', 'World')), c(NA, NA))
})

test_that('convert_str_to_logical doesn\'t trip on NAs, NULLs or empty strings', {
  expect_equal(convert_str_to_logical(c('True', NA)), c(TRUE, NA))
  expect_equal(convert_str_to_logical(c('True', NULL)), c(TRUE))
  expect_equal(convert_str_to_logical(c('True', '')), c(TRUE, NA))
})
