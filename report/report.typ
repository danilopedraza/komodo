#import "@preview/chronos:0.2.0"

#set par(justify: true)
#set text(lang: "es")

#set page(numbering: "1")
// #set page(header: [
//   _El lenguaje de programación Komodo_
//   #h(1fr)
//   // Universidad Nacional de Colombia
// ])

#set heading(numbering: "1.1.1.")

#page(
  align(
    center + horizon,
    [
      #text(17pt)[*El lenguaje de programación Komodo*] \
      \
      \
      César Danilo Pedraza Montoya \
      #link("mailto:cpedraza@unal.edu.co")
    ]
  ),
  numbering: none,
)

#page(outline())

= Introducción

Komodo es un lenguaje de programación hecho para probar ideas rápidamente. Es ideal para problemas con estructuras discretas como números y palabras.
Komodo intenta que operar con estas entidades sea tan fácil como sea posible mientras se minimiza la cantidad de código necesario para llegar
a una implementación exitosa. La otra prioridad de Komodo es la sencillez: se busca que el lenguaje sea pequeño y con reglas simples,
con el propósito de que pueda ser aprendido con facilidad.

Komodo está diseñado con la intención de convertirse en una herramienta útil para generar estructuras discretas que puedan depender
de muchas restricciones, para así estudiarlas. Esta es una tarea común en el estudio de areas de las matemáticas como la combinatoria, la teoría de la computación,
la teoría de grafos o la teoría de códigos. Usar el computador como una herramienta de exploración matemática es una práctica conocida como matemática experimental. @experimental[p.~2]

Este documento describe el lenguaje de programación Komodo. No es una guía de uso del lenguaje. También se exploran detalles del
intérprete de Komodo creado por el autor. Sin embargo, no se describe todo el comportamiento esperado de una implementación del lenguaje,
ni se proveen detalles del intérprete más allá de lo estructural.

= Visión general

El propósito de Komodo tiene consecuencias en su estructura. Puesto que Komodo es un lenguaje para _scripting_, no es una prioridad que el lenguaje
pueda procesarse a si mismo, o que la representación de los datos sea similar a la representación de los programas. Asimismo, el nivel de abstracción de Komodo
y las limitaciones de recursos humanos hacen preferible implementar un intérprete en lugar de un compilador.

El diseño de Komodo no es deliberado, sino que se ha llegado a él con una construcción iterativa.

En esta sección se explican brevemente algunas características de Komodo, que son descritas con mayor detalle en secciones posteriores.

== Sistema de tipos

Komodo es un lenguaje con tipado débil y dinámico. Lo primero significa que las reglas de tipos son relativamente laxas y se realizan conversiones implicitas de tipos, y lo segundo significa que estas reglas y conversiones son verificadas y realizadas en tiempo de ejecución. La razón de esto es que Komodo no fuerza la identificación explícita de tipos, y por lo tanto, en general, no se puede obtener el tipo de un símbolo cualquiera en tiempo de compilación.

Komodo también es de tipado latente, lo que significa que los tipos están asociados a valores y no a variables o símbolos. Esto hace que un símbolo pueda tener valores tipos distintos en momentos distintos. @piercelanguages[p.~2].

== Paradigmas

Komodo emplea dos paradigmas de programación: procedural y funcional.

Por un lado, Komodo es un lenguaje procedural por que las formas y el orden importan: la instancia más importante de esto es que las sentencias de un programa son evaluadas en el
orden en que aparecen en el programa. Esto establece una semántica clara para cambiar el valor de un símbolo, por ejemplo.

Además, Komodo es un lenguaje funcional porque las funciones tienen un papel protagónico: pueden declararse de forma nombrada o anónima, y pueden rastrear patrones.
La búsqueda de patrones de Komodo y su inclusión en las funciones permite describir procedimientos en términos de lo que deben hacer, en lugar de como.

Esto permite que dependiendo del problema, un programa de Komodo pueda ser más imperativo o más declarativo a conveniencia.
La forma en que se restringe la combinación de los dos paradigmas es limitando la mutabilidad de valores.

== La estructura del intérprete

Como suele suceder con múltiples compiladores e intérpretes, el intérprete de Komodo funciona como una cadena de procesamiento. Se comienza procesando texto, y tras cada paso se obtiene una representación del programa más preparada para ser ejecutada. En el caso de Komodo, se tienen las siguientes etapas:

+ Analizador léxico,
+ Analizador sintáctico,
+ Post-analizador sintáctico o _weeder_,
+ Evaluador,
+ Entorno de tiempo de ejecución o _runtime environment_.

Este es un diagrama de secuencia de los componentes del intérprete. Las columnas son los componentes, y las flechas son interfaces. En algunos casos las interfaces son estructuras de datos, y en otros son eventos invocados por el usuario.

#figure(
  chronos.diagram({
    import chronos: *
    _par("Sistema operativo")
    _par("Lexer")
    _par("Parser")
    _par("Weeder")
    _par("Evaluador")
    _par("Runtime")

    _seq("Sistema operativo", "Lexer", comment: "REPL")
    _seq("Sistema operativo", "Lexer", comment: "Archivos con código")
    _seq("Lexer", "Parser", comment: "Token")
    _seq("Parser", "Weeder", comment: "CST")
    _seq("Weeder", "Evaluador", comment: "AST")
    _seq("Evaluador", "Runtime", comment: "Declaraciones")
    _seq("Evaluador", "Runtime", comment: "Referencias")
    _seq("Evaluador", "Runtime", comment: "Objetos")

    _seq("Runtime", "Sistema operativo", comment: "Entrada/salida estándar")
    
  }),
  caption: "Componentes del intérprete y sus relaciones más importantes."
)

En este documento se hace una descripción del funcionamiento de cada componente y cada relación entre componentes, al mismo tiempo que se especifica a Komodo.

= Análisis léxico y sintáctico

== Analizador léxico o _lexer_

  El analizador léxico convierte un programa en una sucesión de _tokens_, que son unidades más complicadas como palabras, números y símbolos. Uno de los propósitos de esta fase es que las demás fases no tengan que lidiar con detalles relacionados al texto que representa el programa: Las fases posteriores no deberían lidiar con aspectos como espacios en blanco, indentación o comentarios en el código. Toda la información necesaria debería estar incluída en los _tokens_ que el analizador emite.

  La entrada del analizador es un _stream_ de caracteres Unicode. Sin embargo, la mayoría de palabras clave y símbolos se componen de caracteres ASCII.

=== Lista de _tokens_
  
  Esta es una lista de los _tokens_ que el analizador léxico emite, y las reglas que hacen que sean emitidos. Se muestran expresiones regulares para algunos _tokens_ con el propósito de ilustrar las reglas rápidamente, pero la implementación del _lexer_ no usa expresiones regulares. Los tokens que están relacionados a los bloques de indentación indentados son casos especiales, cuyo funcionamiento se describe con más detalle en la siguiente sección.

  Las expresiones regulares están escritas con el estilo de Perl. @perlregex

  #{
    show figure: set block(breakable: true)
    figure(
      table(
        columns: 3,
        [Nombre], [Descripción], [Expresión regular],
        [`Ampersand`], [Sígno et: `&`], [`&`],
        [`Arrow`], [Flecha simple: `->`], [`->`],
        [`As`], [Palabra clave: `as`], [`as`],
        [`Assign`], [Símbolo de asignación: `:=`], [`:=`],
        [`Bang`], [Símbolo de exclamación: `!`], [`!`],
        [`Case`], [Palabra clave: `case`], [`case`],
        [`Colon`], [Dos puntos: `:`], [`:`],
        [`Comma`], [Coma: `,`], [`,`],
        [`Dedent`], [El final de un bloque indentado.], [],
        [`Do`], [Palabra clave: `do`], [`do`],
        [`Dot`], [Punto: `.`], [`\.`],
        [`DotDot`], [Punto tras punto: `..`], [`\.\.`],
        [`Else`], [Palabra clave: `else`], [`else`],
        [`Equals`], [Símbolo de igualdad: `=`], [`=`],
        [`False`], [Palabra clave: `false`], [`false`],
        [`FatArrow`], [Flecha gruesa: `=>`], [`=>`],
        [`For`], [Palabra clave: `for`], [`for`],
        [`From`], [Palabra clave: `from`], [`from`],
        [`Greater`], [Símbolo de _mayor que_: `>`], [`>`],
        [`GreaterEqual`], [Símbolo de _mayor o igual que_: `>=`], [`>=`],
        [`Ident`], [Un identificador.], [`\p{Alphabetic}[\p{Alphabetic}\p{GC=Number}]`],
        [`If`], [Palabra clave: `if`], [`if`],
        [`Import`], [Palabra clave: `import`], [`import`],
        [`In`], [Palabra clave: `in`], [`in`],
        [`Indent`], [El inicio de un bloque indentado.], [],
        [`Integer`], [Un entero en base 2, 8, 10 o 16.], [`(0)|(0(b|B)[0-1]+)|(0(o|O)[0-7]+)|([1-9][0-9]*)|(0(x|X)[0-9a-fA-F]+)`],
        [`Lbrace`], [Corchete izquierdo: `{`], [`{`],
        [`Lbrack`], [Paréntesis cuadrado izquierdo: `[`], [`\[`],
        [`LeftShift`], [Dos _menor que_ juntos: `<<`], [`<<`],
        [`Less`], [Símbolo de _menor que_: `<`], [`<`],
        [`LessEqual`], [Símbolo de _menor o igual que_: `<=`], [`<=`],
        [`Let`], [Palabra clave: `let`], [`let`],
        [`LogicAnd`], [Dos sígnos et juntos: `&&`], [`&&`],
        [`LogicOr`], [Dos barras verticales juntas: `||`], [`\|\|`],
        [`Lparen`], [Paréntesis izquierdo: `(`], [`\(`],
        [`Memoize`], [Palabra clave: `memoize`], [`memoize`],
        [`Minus`], [Guión: `-`], [`-`],
        [`Newline`], [Salto de línea.], [],
        [`NotEqual`], [Un _slash_ y un símbolo de igualdad: `/=`], [`\/=`],
        [`Percent`], [Símbolo de porcentaje: `%`], [`%`],
        [`Plus`], [Símbolo de suma: `+`], [`\+`],
        [`Rbrace`], [Corchete derecho: `}`], [`}`],
        [`Rbrack`], [Paréntesis cuadrado derecho: `]`], [`]`],
        [`RightShift`], [Dos _mayor que_ juntos: `>>`], [`>>`],
        [`Rparen`], [Paréntesis derecho: `)`], [`)`],
        [`Slash`], [Una barra inclinada: `/`], [`\/`],
        [`SlashSlash`], [Dos barras inclinadas: `//`], [`\/\/`],
        [`Star`], [Un asterisco: `*`], [`\*`],
        [`StarStar`], [Dos asteriscos: `**`], [`\*\*`],
        [`String`], [Una cadena de caracteres.], [`"[.\w]*"`],
        [`Then`], [Palabra clave: `then`], [`then`],
        [`Tilde`], [Una virgulilla: `~`], [`~`],
        [`True`], [Palabra clave: `true`], [`true`],
        [`Unknown`], [Un caracter no reconocido.], [],
        [`Var`], [Palabra clave: `var`], [`var`],
        [`VerticalBar`], [Barra vertical: `|`], [`\|`],
        [`Wildcard`], [Barra baja: `_`], [`_`],
      ),
      caption: [Tokens del _lexer_ de Komodo y sus reglas.]
    )
  }

Hay algunas particularidades a mencionar:

+ El _lexer_ ignora los segmentos que comienzan con un númeral `#` y terminan con un salto de línea. Estos son los comentarios de Komodo.

+ Los identificadores reciben toda la clase `Alphabetic` de Unicode en su primer caracter, y luego reciben caracteres de la clase `Alphabetic` o `Number`. @unicodepropslist
  
  Estos nombres son propiedades de caracteres Unicode. @unicodeprops

+ Como muestra su expresión regular, los identificadores no incluyen barras bajas en ningún punto. Están exlusivamente compuestos de caracteres alfanuméricos.

+ Los ceros a la izquierda en enteros decimales no están permitidos. Un cero sólo va al principio de un _token_ `Integer` cuando consiste en un solo cero, o cuando se va a escribir un prefijo para una base numérica no decimal (`0b`, `0o` o `0x`).

=== Rastreo de indentación y el alcance del analizador léxico

Los _tokens_ `Indent` y `Dedent` indican el inicio y el final de un bloque de código indentado, respectivamente. Por ejemplo, en el siguiente fragmento de código

```
let f(x) := x + 2
```

el cuerpo de la función `f` está compuesto exactamente por la expresión `x + 2`. Si se requiere que el cuerpo de la función tenga más líneas de código, se puede iniciar un bloque en una nueva línea. Las líneas que pertenecen al bloque están espaciadas a la derecha por 4 espacios:

```
let f(x) :=
    let y := 2
    x + y
```

En este caso, la función `f` esta compuesta por un bloque de código de dos líneas.

Para indicar el inicio de este bloque, el _lexer_ emite un token `Indent` antes de emitir los correspondientes al mismo. Tras haber emitido todos los _tokens_ del bloque, se emite un `Dedent` para indicar el fin de este.

Este comportamiento también ocurre cuando hay bloques dentro de bloques, como en el siguiente programa:

```
for i in 0..10 do
    # se emite el primer Indent
    for j in 0..10 do
        # se emite el segundo Indent
        mat[i][j] = i + j
# Se emiten dos Dedent seguidos
```

En este caso, el cuerpo del primer ciclo `for` es un un bloque de código, cuya única parte es otro ciclo `for`, cuyo cuerpo es otro bloque de una sola línea. Después del primer `do` se emite un `Indent`. Después del segundo `do` se emite otro `Indent`. Cuando se llega al final del texto, se emiten dos `Dedent` seguidos para "cerrar" los dos bloques de código que estaban "abiertos".

La razón para hacer esto es que al emitir estos _tokens_ se pueden entender los bloques de código de la misma forma que se hace con lenguajes donde los bloques están delimitados con caracteres como corchetes. Por ejemplo en JavaScript, este es un programa similar:

```
for (let i = 0; i < 10; i++) {
    for (let j = 0; j < 10; j++) {
        mat[i][j] = i + j;
    }
}
```

Los corchetes aquí cumplen la misma función que los `Indent` y `Dedent` en Komodo, solo que en este caso tienen una correspondencia directa con caracteres del texto. En el caso de Komodo son un artificio obtenido de contar espacios en blanco.

Esta es una descripción de como el _lexer_ decide emitir estos _tokens_:

+ El _lexer_ cuenta el nivel de indentación en el que se encuentra el programa en el punto en donde el texto está siendo leído. Cuando se está al principio del programa, este nivel es cero.

+ Cuando se llega a una nueva línea, se quiere contar su nivel de indentación. Esto se hace contando el número de espacios al principio de la línea. Cada 4 espacios son un nivel de indentación. Si quedan espacios sobrantes (es decir, el número de espacios no es múltiplo de 4), se ignora el residuo.
  
  Por ejemplo, la línea de código
  ```
          println(x)
  ```

  tiene 8 espacios al principio, por lo que su nivel de indentación es 2.

  Las líneas que estan exclusivamente de espacios o espacios con comentarios son ignoradas.

+ Una vez que se consumen y cuentan los espacios, y que se llega a un caracter que va a componer un _token_, se compara el nivel de indentación de la línea con el nivel de indentación que el _lexer_ guarda.

  - Si son iguales, no se emiten _tokens_ de más: el resto de la línea es consumida.

  - Si el nivel de la línea es mayor, se emite tantos `Indennt` como la diferencia entre el nivel de la línea y el nivel guardado en el _lexer_, y se consume el resto de la línea.

  - Si el nivel de la línea es menor, se emiten tantos `Dedent` como la diferencia entre el nivel guardado en el _lexer_ y el nivel de la línea. Luego se consume el resto de la línea.

Cabe destacar que la razón por la que hay que almacenar el nivel de indentación es por que las reglas con las que estos _tokens_ son emitidos son dependientes del contexto: no basta con conocer el caracter actual o una cantidad fija hacia adelante, sino, en general, es necesario poder recorrer todos los caracteres recorridos antes. Una solución más sensible es almacenar un estado útil (el nivel de indentación) para poder decidir cuando emitir los _tokens_.

== Analizador sintáctico

  El analizador sintáctico convierte sucesiones de tokens en un árbol que describe la estructura sintáctica del programa, conocido como CST (del inglés _Concrete Syntax Tree_). Este árbol contiene todos los detalles del programa, y es generado casi en su totalidad de forma independiente del contexto. La mayoría de la estructura del programa se obtiene de este paso.

== Post-analizador sintáctico o _weeder_

  El _weeder_ toma un CST y realiza dos tareas:

  - Eliminar detalles innecesarios para la evaluación del código,
  - Verificar condiciones del programa que serían más dificiles de verificar en etapas anteriores.

  El resultado es un árbol de sintaxis abstracto o AST (del inglés _Abstract Syntax Tree_), que no contiene detalles como la precedencia de operadores, espacios o indentación. También convierte ciertos operadores infijos en nodos más restringidos, para facilitar la evaluación y eliminar estados indeseables. El tipo de errores que el _weeder_ captura son de naturaleza sintáctica y dependientes del contexto.

= Ejecución de programas

== Evaluador

  El evaluador toma nodos del AST y los convierte en valores o acciones. Komodo es un lenguaje orientado a expresiones,
  por lo que las evaluaciones siempre retornan un valor. Las acciones se dan en un entorno que es modificado cuando los nodos del AST son evaluados.

== Variables

== _Pattern matching_

== Entorno de tiempo de ejecución

  El entorno de tiempo de ejecución es la representación de los tipos de Komodo y un modelo de memoria donde se almacenan estas representaciones. Conforme los nodos son evaluados, el entorno es modificado. El modelo de memoria es una pila de _scopes_, donde se añaden _scopes_ nuevos si se entra a un bloque de código nuevo.
  Todas las llamadas a funciones tienen un _scope_ propio. Todas los bloques indentados tienen un scope propio.

== Tipos

=== Números

Komodo tiene tres representaciones para números: Enteros, decimales y fracciones. Todos tienen tamaño arbitrario, que crece bajo demanda.

==== Enteros

Los enteros tienen signo y tienen las operaciones de suma, multiplicación, división, residuo, exponenciación y corrimiento de bits, tanto a la izquierda como a la derecha. Los bits más significativos están a la izquierda. Son representados en tiempo de ejecución como arreglos dinámicos de enteros de longitud de la palabra de máquina.

==== Decimales

Los decimales tienen signo y tienen las operaciones de suma, multiplicación y división. Son representados como enteros de longitud arbitraria y un entero de longitud de máquina, que representa la ubicación del punto en el número entero.

==== Fracciones

Las fracciones tienen signo y tienen las operaciones de suma, multiplicación, división y exponenciación. Son representados como un par de enteros de longitud arbitraria.

=== Funciones

Las funciones de Komodo pueden escribirse de dos formas:

- De forma anónima, como una lista de parámetros y un bloque de código,
- con nombre, como una lista de patrones y una lista de bloques de código correspondiente.

Las funciones de Komodo pueden ser pasadas como argumentos de otras funciones.

Los _scopes_ de Komodo son creados de forma léxica, lo que significa que los nombres referenciados en la función son los que se obtienen en el contexto de la definición de la función, y no en el contexto de sus ejecuciones.

=== Caracteres y cadenas

==== Caracteres

Los caracteres de Komodo son valores escalares de Unicode, por lo que pueden representar exactamente los valores descritos por el estándar UTF-8. Tienen una longitud fija 32 bits.

==== Cadenas

Las cadenas de Komodo están representadas como arreglos inmutables de bytes, que están codificados con UTF-8.

Se puede iterar de izquierda a derecha sobre las cadenas de Komodo de la misma forma que se hace con las listas.

=== Contenedores

Los contenedores almacenan otros valores, incluyendo los de su mismo tipo. Todos los contenedores de Komodo permiten almacenar valores de diferente tipo en el mismo contenedor simultáneamente.

==== Listas

Las listas de Komodo son de longitud arbitraria y puede accederse a sus elementos de dos formas:

- con índices enteros indexados desde cero,
- iterando sobre la lista de izquierda a derecha.

Están representadas como arreglos dinámicos.

==== Conjuntos

Los conjuntos de Komodo son de longitud arbitraria. Se puede iterar sobre ellos y verificar la pertenencia de elementos.

Están representados como árboles binarios de búsqueda.

==== Diccionarios - Objetos

Los diccionarios de Komodo son de longitud arbitraria. Son colecciones de parejas clave-valor, donde el tipo de ambos es arbitrario.

Se puede acceder a sus elementos de dos formas:

- Notación de objeto: `objeto.clave`, donde `objeto` es un diccionario y `clave` es interpretado como una cadena, que es buscada en el diccionario.
- Notación usual: `dic[clave]` donde `dic` es un diccionario y `clave` es un valor arbitrario. 

Están representados como árboles binarios de búsqueda.

== Patrones

El mecanismo principal para representar lógica en Komodo es la búsqueda de patrones o _pattern matching_.
Esto consiste en contrastar valores con patrones. Si un valor es compatible con un patrón, se ejecuta el código asociado a ese patrón.

Toda expresión que represente un valor de Komodo también es un patrón. Toda expresión que contenga un comodín (representado en Komodo con la barra baja "`_`")
es un patrón. Las listas y conjuntos son compatibles con un patrón especial que permite iterar sobre ellos, conocido popularmente como notación `cons`.

La verificación de patrones está implementada comparando el árbol de sintaxis del patrón en cuestión con el valor que se quiere comparar.

== El intérprete

El intérprete de Komodo es un binario compilado estáticamente. Además de la interfaz del sistema operativo, el intérprete no tiene dependencias en tiempo de ejecución.

El intérprete está escrito en el lenguaje de programación Rust. El ecosistema de Rust, de manera similar a lenguajes como OCaml, es favorable para construir herramientas para lenguajes de programación. El modelo de memoria de Rust no incluye manejo de memoria automático, sino un sistema que permite verificar reglas que garantizan seguridad de memoria en tiempo de compilación.

== Módulos de código

Komodo permite importar otros archivos con código, ya sea de fuentes externas o de la librería estándar.

=== Librería estándar

Komodo tiene una pequeña librería estándar que se incluye con la instalación del intérprete.

=== Importación de código externo

Se pueden importar archivos arbitrarios que serán interpretados como programas de Komodo, y de los que se extraerán nombres. Se pueden usar rutas relativas del sistema operativo, y se usará la ubicación del archivo ejecutado actualmente como referencia.

== Gestión de memoria

El intérprete gestiona la memoria automáticamente con un algoritmo de _mark-and-sweep_ sin adiciones. El espacio de memoria crece a demanda en tiempo de ejecución.

== Interacción con el sistema

Hasta ahora, Komodo sólo interactúa directamente con la entrada y salida estándar, e indirectamente con la importación de código.

= Aspectos periféricos

Hay software adicional al intérprete que lo asiste o extiende su alcance.

== Editor web

Una compilación del interprete a _Web Assembly_ es usada para interactuar poder usar el intérprete en navegadores de Internet. Es una versión sin
la librería estándar y con una interfaz simulada de la entrada y salida estándar.

== Resaltado de sintaxis

Se escribió una gramática de _TextMate_ para los _tokens_ de Komodo, y así obtener resaltado de sintaxis en los editores de texto compatibles.

#bibliography("ref.bib", title: "Referencias")
