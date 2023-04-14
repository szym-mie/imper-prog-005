#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#define SIZE 40

#define TEST 1     // 1 - dla testow,  0 - dla oceny automatycznej

// 5.1.1

#define STRLEN_MAX  SIZE
#define KW_NO   32    // liczba zapisanych lancuchow (slow kluczowych)

// n lancuchów wskazywanych w tablicy wskaznikow ptab kopiuje do tablicy tablic t2D   
void n_str_copy(char t2D[][STRLEN_MAX], char *ptab[], size_t n) {
	for (int i = 0; i < n; i++)
	{
		int j = 0;
		char *t = ptab[i];
		char *t2 = t2D[i];

		while (*t != '\0' && j < STRLEN_MAX - 1)
		{
			*t2++ = *t++;
			j++;
		}
		*t2 = '\0';
	}
}

int compar(const void *p1, const void *p2) {
	char *c1 = *((char **)p1);
	char *c2 = *((char **)p2);

	while (*c1 != '\0' && *c1 == *c2)
	{
		c1++;
		c2++;
	}

	return *c1 - *c2;
}

struct ndx_str
{
	size_t ndx;
	const char *str;
};

int compar_ndx_str_rev(const void *p1, const void *p2)
{
	struct ndx_str *s1 = (struct ndx_str *)p1;
	struct ndx_str *s2 = (struct ndx_str *)p2;

	char *c1 = s1->str;
	char *c2 = s2->str;

	while (*c1 != '\0' && *c1 == *c2)
	{
		c1++;
		c2++;
	}

	return *c2 - *c1;
}

// sortuje alfabetycznie n lancuchow wskazywanych w tablicy wskaznikow t  
void ptab_sort(char *ptab[], size_t n) {
	qsort(ptab, n, sizeof(char *), compar);
}

// Porzadek odwrotny do alfabetycznego lancuchow zapisanych w tablicy t2D zapisuje w tablicy indices
void t2D_sort(const char t2D[][STRLEN_MAX], size_t indices[], size_t n) {
	struct ndx_str *strs = malloc(sizeof(struct ndx_str) * n);
	if (strs == NULL) return;
	struct ndx_str *cptr = strs;

	for (size_t i = 0; i < n; i++)
	{
		cptr->ndx = i;
		cptr->str = t2D[i];
		cptr++;
	}

	qsort(strs, n, sizeof(struct ndx_str), compar_ndx_str_rev);
	
	cptr = strs;
	for (size_t i = 0; i < n; i++)
		indices[i] = cptr++->ndx;

	free(strs);
}

// W wierszach tablicy t2D sa zapisane lancuchy znakowe w dowolnej kolejnosci.
// Tablica indeksow wierszy indices wyznacza porzadek tych lancuchow.
// Funkcja wypisuje w osobnych liniach łańcuchy wskazane przez n poczatkowych elementów tablicy indices.
void print_t2D_ind(const char (*ptr)[STRLEN_MAX], const size_t *pindices, size_t n) {
	for (size_t i = 0; i < n; i++) 
	{
		puts(*(ptr+(*pindices)));
		pindices++;
	}
}

// Funkcja wypisuje w osobnych liniach n łańcuchów wskazywanych przez elementy tablicy ptab.
void print_ptab(char *ptab[], size_t n) {
	for (int i = 0; i < n; i++)
		puts(ptab[i]);
}

// 5.1.2
// A mxp, B pxn

void mat_product(const double A[][SIZE], const double B[][SIZE], double AB[][SIZE], size_t m, size_t p, size_t n) {
	for (int i = 0; i < m; i++)
	{
		for (int j = 0; j < n; j++)
		{
			double sum = 0;
			for (int k = 0; k < p; k++)
			{
				sum += A[i][k] * B[k][j];
			}
			AB[i][j] = sum;
		}
	}
}


// 5.1.3 Triangulatyzacja macierzy i obliczanie wyznacznika - wersja uproszczona
// (bez zamiany wierszy).
// Jezeli element glowny a[i][i] = 0, to triangularyzacja nie jest dokonczona,
// a wyznacznik = NAN
// Zalozenie: funkcja gauss moze zmienic wartosci elementow tablicy A

double gauss_simplified(double A[][SIZE], size_t n) {
	int pr = 0;
	int pc = 0;

	while (pr < n && pc < n)
	{
		int max_i = pr; // TODO
		for (int i = pr + 1; i < n; i++)
		{
			if (fabs(A[i][pc]) > fabs(A[max_i][pc])) max_i = i;
		}
		if (A[max_i][pc] == 0) pc++;
		else
		{
			for (int k = 0; k < n; k++)
			{
				double t = A[pr][k];
				A[pr][k] = A[max_i][k];
				A[max_i][k] = t;
			}
			for (int i = pr + 1; i < n; i++)
			{
				double f = A[i][pc] / A[pr][pc];
				A[i][pc] = 0;
				for (int j = pc + 1; j < n; j++)
					A[i][j] -= A[pr][j] * f;
			}
			pr++;
			pc++;
		}
	}

	double s = -1;
	for (int pp = 0; pp < n; pp++)
		s *= A[pp][pp];
	
	return s;
}

void read_mat(double A[][SIZE], size_t m, size_t n) {
  for(size_t i=0; i<m; ++i) {
    for(size_t j=0; j<n; ++j)  scanf("%lf",&A[i][j]);
  }
}

void print_mat(const double A[][SIZE], size_t m, size_t n) {
  for(size_t i=0; i<m; ++i) {
    for(size_t j=0; j<n; ++j)  printf("%.4f ",A[i][j]);
    printf("\n");
  }
}

int main(void) {
    double A[SIZE][SIZE], B[SIZE][SIZE], C[SIZE][SIZE];
    double b[SIZE], x[SIZE];

	int to_do;
	size_t m,n,p,q;

	char *keywords_ptab[] = {"do", "struct", "typedef", "for", "union",
    "int", "void", "long", "register", "auto", "return",
    "double", "else", "sizeof", "enum", "const", "continue",
    "default", "short", "extern", "static", "float",
    "goto", "switch", "if", "unsigned", "volatile",
    "while", "signed", "break", "char", "case"};
    char keywords_t2D[KW_NO][STRLEN_MAX];
    size_t indices[KW_NO];

	if(TEST) printf("Wpisz nr zadania ");
	scanf ("%d", &to_do);
	switch (to_do) {
    case 1:
      n_str_copy(keywords_t2D, keywords_ptab, KW_NO);
      puts("copied");
      ptab_sort(keywords_ptab, KW_NO);
      puts("ptab sort");
      t2D_sort(keywords_t2D, indices, KW_NO);
      puts("t2D sort");
      if(TEST) printf("Wpisz liczbe lancuchow n ");
      scanf("%d", &n);   
      print_ptab(keywords_ptab, n);
      print_t2D_ind(keywords_t2D, indices, n);
      break;
    case 2:
      if(TEST) printf("Wpisz liczbe wierszy i liczbe kolumn mac.1: ");
      scanf("%d %d", &m, &n);
      if(TEST) printf("Wpisz mac.1 (wierszami): ");
      read_mat(A,m,n);
      if(TEST) printf("Wpisz liczbe wierszy i liczbe kolumn mac.2: ");
      scanf("%d %d", &p, &q);
      if(TEST) printf("Wpisz mac.2 (wierszami): ");
      read_mat(B,p,q);
      if(n == p) mat_product(A,B,C,m,n,q);
      print_mat(C,m,q);
      break;
    case 3:
      if(TEST) printf("Wpisz liczbe wierszy i kolumn mac. kwadratowej: ");
      scanf("%d", &n);
      if(TEST) printf("Wpisz macierz A (wierszami): ");
      read_mat(A,n,n);
      printf("%.4f\n", gauss_simplified(A, n));
      print_mat(A,n,n);
      break;
    default:
      printf("NOTHING TO DO FOR %d\n", to_do);
  }
  return 0;
}


