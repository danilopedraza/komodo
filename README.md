# Symstatic

Una calculadora simbólica.

# Sobre el proyecto

## Motivaciones

Llevo mucho tiempo pensando en un sistema que me permita hacer conjeturas matemáticas rápido y sin introducir demasiados detalles que me alejen del problema inicial. Lo que se suele utilizar en estos casos son paquetes como Mathematica, Maple o Sage. Hay cosas que todos estos tienen:

- Son muy grandes, con muchas funcionalidades.
- La sintaxis de sus lenguajes no me gusta mucho. La de Sage me gusta, pero es el sistema que menos conozco.
- Nunca son realmente cómodos. Esto tiene sentido, considerando que no son específicos a ningún área de las matemáticas. Debo imaginar que nadie está del todo cómodo.

Además, Mathematica y Maple no son de código abierto.

Al principio pensé que las palabras clave del lenguaje estuvieran en español (¡por esto este README está en español!). Cambié de opinión.

## Precedentes

Hay dos lenguajes que me parecen una buena forma de partir: [Miranda](https://www.cs.kent.ac.uk/people/staff/dat/miranda/) y [SETL](https://setl.org/setl/). Aún no estoy seguro de si este lenguaje será completamente funcional. Tal vez no. Las cosas que más me interesa incluir son:

- Laziness
- Pattern matching robusto
- List/Set comprehension

Además, si termino enfocando esto a problemas de matemáticas discretas, el compilador tendrá que poder optimizar diversas operaciones recursivas.

Lo que más quiero sacar de estos lenguajes de su expresividad. Como este es un lenguaje para hacer experimentos de matemáticas, no tengo gran interés en rastrear errores en tiempo de compilación. El sistema de tipos y la expresividad de los mismos sólo existe para ayudar al usuario a específicar las propiedades de los símbolos que manipula. Por ejemplo, si el usuario desea integrar simbólicamente una expresión pero no está interesado en obtener funciones en los complejos como resultado, la idea es que esto sea fácil de expresar a través del sistema de tipos.

Recientemente me crucé con [Picat](https://picat-lang.org/), un lenguaje con símbolos, predicados y pattern matching. También tiene utilidades para resolver CSPs, problemas de planeación y cosas por el estilo. Es casi como lo que busco con este lenguaje.
