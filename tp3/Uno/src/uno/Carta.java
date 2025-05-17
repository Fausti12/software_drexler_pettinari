package uno;

import java.util.function.Function;

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
    public abstract void accionSobre(Juego juego);
    public abstract boolean puedeSerJugadoSobre(Carta carta);
    // Protocolo de aceptación
    public boolean teGustaColor(Color color) {return this.color == color;}
    public boolean teGustaNumero(int numero) {return false;}
    public boolean teGustaMiTipo(Class<? extends Carta> tipo) {return this.getClass() == tipo;}

    public void asignarColor(Color color) {throw new Error();}

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
        //si otra carta no tiene numero devuelve false en 2do término (default clase)
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
        juego.cambiarDireccion();
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
        juego.avanzarTurno();
    }

    public boolean puedeSerJugadoSobre(Carta otra) {
        return otra.teGustaColor(this.color) || otra.teGustaMiTipo(this.getClass());
    }

    public String toString() {
        return "SKIP " + color;
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

    public boolean teGustaColor(Color color) { return color == this.elegido;}

    public void asignarColor(Color color) { elegido = color; }

    public Color color() {return elegido;}

    public String toString() { return "WILD(" + elegido + ")";}
}


class CartaWildDraw4 extends Carta {
    private Color elegido = Color.NINGUNO;

    public CartaWildDraw4() {
        super(Color.NINGUNO);
    }

    public void accionSobre(Juego juego) {
        // juego.elegirColor();
    }

    public boolean puedeSerJugadoSobre(Carta carta) { return true;}

    public boolean teGustaColor(Color color) { return color == this.elegido;}

    public void asignarColor(Color color) {elegido = color;}

    public Color color() {return elegido;}

    public String toString() { return "WILD(" + elegido + ")";}
}
