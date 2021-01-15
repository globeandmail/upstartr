test_that('index indexes correctly', {
  expect_equal(index(c(20, 40, 10)), c(0, 1, -0.5))
  expect_equal(index(c(20, 40, 10), base = 100), c(100, 200, 50))
})

test_that('index correctly uses other bases', {
  expect_equal(index(c(20, 40, 10), base = 100), c(100, 200, 50))
})

test_that('mode returns correct mode', {
  expect_equal(mode(c(1, 1, 1, 2, 2, 3)), 1)
  expect_equal(mode(c('Jane', 'John', 'Stewart', 'Jane', 'Tom')), 'Jane')
  expect_equal(mode(c(TRUE, FALSE, TRUE)), TRUE)
  expect_equal(mode(c(0.1, 0.1, 0.2)), 0.1)
})

test_that('mode returns multiple modes if there are ties', {
  expect_equal(mode(c(1, 1, 1, 2, 2, 2, 3)), c(1, 2))
  expect_equal(mode(c(1, 2)), c(1, 2))
  expect_equal(mode(c('Tom', 'Jane')), c('Tom', 'Jane'))
  expect_equal(mode(c(TRUE, FALSE)), c(TRUE, FALSE))
})

# test_that('unaccent de-accents words', {
#
# })
#
# test_that('remove_non_utf8 replaces non-UTF-8 characters with empty strings', {
#
# })

test_that('%not_in% returns a logical vector for items from A not in B', {
  expect_equal(c(1, 2, 3, 4, 5) %not_in% c(3, 4, 5, 6, 7), c(TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_equal(c('The', 'quick', 'brown', 'fox') %not_in% c('fox', 'jumped', 'over', 'the', 'lazy', 'dog'), c(TRUE, TRUE, TRUE, FALSE))
})

test_that('not.na returns a logical vector for items that are not NA', {
  expect_equal(not.na(c(1, 2, 3, NA, 4, 5, NA, 6)), c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE))
  expect_equal(not.na(c('Hello', NA, 'World')), c(TRUE, FALSE, TRUE))
  expect_equal(not.na(c(NA, TRUE, FALSE)), c(FALSE, TRUE, TRUE))
})

# simplify_string
# clean_columns
# convert_str_to_logical
