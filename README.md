# guaguaUPM

Repositorio para los trabajos de grupo de Infórmatica del Grado de Ingeniería Aeroespacial de la UPM, curso 2017-2018 grupo M6.

## Integrantes:

* Fernando A.
* Álvaro G.
* Alejandro C.
* Lucas
* Fran L.

## Uso de los códigos:

Para poder usar la ibrería donde se guardan módulos comunes, primero crear la carpeta donde se guardará:

```
mkdir $HOME/libDIOS
```

Hemos optado por usar esa direccion en vez de /usr/lib para no tener que usar permisos de superusario en macOS para poder actualizar la libreria.

Para usar los makefile:

```
make all
```
Compilará la librería y el programa de la carpeta de trabajo, mientras que

```
make
```
solo compilará los programas de la carpeta de trabajo.

Para limpiar todos los ejecutables, simplemente usar el archivo clean.sh

### Ejemplos de subrutinas

```
write_AB(A,B,N)
```
Mostrará por pantalla un sistema (A|B) con A siendo una matriz real8 NxN y B un vector real8 N.

```
resolver_gauss(A,B,X,N)
resolver_LU(A,B,X,N)
resolver_LAPACK(A,B,X,N)
```
Resuelven sistemas lineales AX=B de distintos métodos.