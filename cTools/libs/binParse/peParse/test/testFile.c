void shortNam(void);
void shortNam(void)
{
    asm volatile ("label:" ::: "memory");
}

void longNameTest(void);
void longNameTest(void)
{
    asm volatile ("long_test_label:" ::: "memory");
}

int main(void)
{
    return 0;
}

