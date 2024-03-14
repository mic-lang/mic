lifetime <depth p, depth q, kind a>
static struct X<p, q> func(struct Y a, int b) using p {
    struct Y {int x;};
    return (struct X){0};
}
struct X;
struct Y;
struct X<static, dyn> dyn* x;

lifetime <depth p, kind a>
struct X {
    void p a* x;
};

struct X<static, dyn>* y;

extern int printf(const char *, ...);

int main () using p {
    func<>;;
    //struct X<p, dyn> *const b = func<p, dyn>((struct X){.x=0});
    goto label;
    //struct Y p* a;
    //struct Y{int p* a;} p auto* b;
    int a;
    {
        int b;
        b = a;
        a;
    }
    a;
    //*&*&(b->a);
    //&5;
    func<>;
    int c[4];
    //a = c[3];
    //c;

label : 
    for (int i=0;i<5;i++) using q {
        (*printf)("%d");
        if (++i==4)
        break;
        if (i<5)continue;
    }
    switch (0) using q {
        case 0:
        case 1:
        default:
        int printf = 0;
        break;
    }
return 0;
}
char c = '\n';
unsigned i = 5;
struct X x= {.x=0,};
char* var[4+5] = "abc";
int* const x;

typedef struct X Z;
Z;
void(*func)(int a);
char *const ptr [4] ;
char (*const ptrq)[4];
void (*signal (int sig, void(*func)(int a)))(int b);
void (*(*p)[10])(int a, int b);
struct X x = (struct X){.x=0,};