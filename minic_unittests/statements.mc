private int256 foo() {
	int256 tmp;

	tmp = 0;
	return tmp;
}

void bar(int256 tmp) {
	tmp = 0;
	transfer(0, 1);

	foo();
	tmp <- foo();

	{
		tmp = 0;
	}

	if (tmp == 0) {
	}

	if (tmp == 0)
		tmp = 1;

	if (tmp == 0)
		tmp = 1;
	else
		tmp = 2;

	if (tmp == 0) {
		tmp = 1;
	} else {
		tmp = 2;
	}

	while
		tmp = tmp - 1;

	while {
		tmp = tmp - 1;
	}

	while {
		tmp = tmp - 1;
		foo();
		if (tmp == 0) break;
	}

	while {
		tmp <- foo();
		if (tmp == 0) break;
	}

	callmethod(tmp; tmp; 12345; 6; ; 7);

	revert;
	return;
	return tmp;

}
