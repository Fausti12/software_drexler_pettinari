package uno;

enum Color {
    ROJO, AZUL, VERDE, AMARILLO, NINGUNO;
}

abstract class Carta {
    protected final Color color;
    //private boolean cantadoUno = false;

    public Carta(Color color) {
        this.color = color;
    }

    public Color color() {
        return color;
    }
/*
    public boolean fueCantadoUno() {
        return cantadoUno;
    }

    public Carta uno() {
        this.cantadoUno = true;
        return this;
    }
*/
    public abstract void aplicarEfecto(Juego juego);
    public abstract boolean puedeSerJugadoSobre(Carta carta);
}

class CartaNumero extends Carta {
    private final int numero;

    public CartaNumero(Color color, int numero) {
        super(color);
        this.numero = numero;
    }

    public int numero() {
        return numero;
    }

    public void aplicarEfecto(Juego juego) {
        // no tiene efecto especial
    }


    public boolean puedeSerJugadoSobre(Carta carta) {  //despues usar double dispatch
        if (carta instanceof CartaNumero otra) {
            return this.color == carta.color() || this.numero == otra.numero();
        }
        return this.color == carta.color();
    }

    public String toString() {
        return color + " " + numero;
    }
}

class CartaDraw2 extends Carta {
    public CartaDraw2(Color color) {
        super(color);
    }

    public void aplicarEfecto(Juego juego) {
        //juego.siguienteJugador().robar(juego, 2);
        //juego.saltarJugador();
        return ;
    }

    public boolean puedeSerJugadoSobre(Carta carta) {
        return this.color == carta.color() || carta instanceof CartaDraw2;
    }

    public String toString() {
        return color + " +2";
    }
}

class CartaReverse extends Carta {
    public CartaReverse(Color color) {
        super(color);
    }


    public void aplicarEfecto(Juego juego) {
       // juego.cambiarDireccion();
    }

    public boolean puedeSerJugadoSobre(Carta carta) {
        return this.color == carta.color() || carta instanceof CartaReverse;
    }

    public String toString() {
        return color + " Reverse";
    }
}

class CartaSkip extends Carta {
    public CartaSkip(Color color) {
        super(color);
    }


    public void aplicarEfecto(Juego juego) {
        //juego.saltarJugador();
    }

    public boolean puedeSerJugadoSobre(Carta carta) {
        return this.color == carta.color() || carta instanceof CartaSkip;
    }

    public String toString() {
        return color + " Skip";
    }
}

class CartaWild extends Carta {
    private Color elegido = Color.NINGUNO;

    public CartaWild() {
        super(Color.NINGUNO);
    }

    public void aplicarEfecto(Juego juego) {
       // elegido = juego.elegirColor();
    }

    public boolean puedeSerJugadoSobre(Carta carta) {
        return true;
    }

    public Color color() {
        return elegido;
    }

    public String toString() {
        return "WILD(" + elegido + ")";
    }
}
