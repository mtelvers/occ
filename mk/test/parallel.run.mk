# A failure under -j stops what depended on it, and the recipes already
# running are waited for. 
# flags: -j4
# goals: all
all: one two three
# the siblings take long enough that they are still running when the
# failure is noticed, so that the message about waiting for them is
# there in both makes rather than depending on the timing
one:
	@sleep 0.4; echo 1 > made-one
two:
	@false
three:
	@sleep 0.4; echo 3 > made-three
