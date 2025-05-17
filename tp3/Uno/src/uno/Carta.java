package uno;

import java.util.function.Function;

enum Color {
    ROJO, AZUL, VERDE, AMARILLO, NINGUNO;
}

/*
interface AccionJuego {
    void robar(int cantidad);
    void saltar();
    void cambiarDireccion();
    String siguienteJugadorNombre();
}
*/

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
    public abstract void accionSobre(Juego juego);
    public abstract boolean puedeSerJugadoSobre(Carta carta);
    // Protocolo de aceptación
    public boolean teGustaColor(Color color) {return this.color == color;}
    public boolean teGustaNumero(int numero) {return false;}
    public boolean teGustaMiTipo(Class<? extends Carta> tipo) {return this.getClass() == tipo;}

    public int numero() {
        return -1; // valor inválido por defecto
    }
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

    public boolean teGustaNumero(int numero) { return this.numero == numero;}

    public boolean puedeSerJugadoSobre(Carta otra) {
        return otra.teGustaColor(this.color) || otra.teGustaNumero(this.numero) ;
        //si otra carta no tiene numero, devuelve false (default clase)
    }

    public void accionSobre(Juego juego) {
        // Sin efecto especial
    }

    public String toString() {
        return color + " " + numero;
    }
}

class CartaDraw2 extends Carta {
    public CartaDraw2(Color color) {super(color);}

    public void accionSobre(Juego juego) {
        juego.robar(2);
        juego.avanzarTurno();
    }

    public boolean puedeSerJugadoSobre(Carta otra) {
        return otra.teGustaColor(this.color) || otra.teGustaMiTipo(this.getClass());
    }

    public String toString() {return color + " draw2";}
}

class CartaReverse extends Carta {
    public CartaReverse(Color color) {super(color);}

    public void accionSobre(Juego juego) {
        // Podría invertir dirección si se implementa
    }

    public boolean puedeSerJugadoSobre(Carta otra) {
        return otra.teGustaColor(this.color) || otra.teGustaMiTipo(this.getClass());
    }

    public String toString() {
        return color + " Reverse";
    }
}

class CartaSkip extends Carta {
    public CartaSkip(Color color) {super(color);}

    public void accionSobre(Juego juego) {
        juego.pasaTurno();
    }

    public boolean puedeSerJugadoSobre(Carta otra) {
        return otra.teGustaColor(this.color) || otra.teGustaMiTipo(this.getClass());
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

    public void accionSobre(Juego juego) {
        // juego.elegirColor();
    }

    public boolean puedeSerJugadoSobre(Carta carta) { return true;}

    public Color color() {return elegido;}

    public String toString() { return "WILD(" + elegido + ")";}
}
