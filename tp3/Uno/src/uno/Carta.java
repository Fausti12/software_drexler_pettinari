package uno;

import java.util.function.Function;

enum Color {
    ROJO, AZUL, VERDE, AMARILLO, NINGUNO;
}


abstract class Carta {
    protected Color color;
    private boolean cantadoUno = false;

    public Carta(Color color) {
        this.color = color;
    }

    public Color color() {
        return color;
    }
    public int numero() {throw new Error();}

    public Carta uno() {
        this.cantadoUno = true;
        return this;
    }

    public boolean fueCantadoUno() { return cantadoUno; }

    public void accionInicial(Juego juego){} ;
    public abstract void accionSobre(Juego juego);
    public boolean puedeSerJugadoSobre(Carta otra) { //default: skip, reverse, draw2
        return otra.teGustaColor(this.color) || otra.teGustaMiTipo(this.getClass());
    }

    // Protocolo de aceptación
    public boolean teGustaColor(Color color) { return this.color == color; }
    public boolean teGustaNumero(int numero) { return false; }
    public boolean teGustaMiTipo(Class<? extends Carta> tipo) { return this.getClass() == tipo; }

    public Carta asignarColor(Color color) { throw new Error(); }

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
    }

    public void accionSobre(Juego juego) {
        juego.avanzarTurno();
    }

    public String toString() {
        return color + " " + numero;
    }
}


class CartaDraw2 extends Carta {
    public CartaDraw2(Color color) {super(color);}

    public void accionInicial(Juego juego){
        juego.robar(2);
        juego.avanzarTurno();
    }

    public void accionSobre(Juego juego) {
        juego.avanzarTurno();
        juego.robar(2);
        juego.avanzarTurno();
    }

    public String toString() {return color + " draw2";}
}

class CartaReverse extends Carta {
    public CartaReverse(Color color) {super(color);}

    //accion inicial no implementado

    public void accionSobre(Juego juego) {
        juego.cambiarDireccion();
        juego.avanzarTurno();
    }

    public String toString() {
        return color + " Reverse";
    }
}


class CartaSkip extends Carta {
    public CartaSkip(Color color) {super(color);}

    public void accionInicial(Juego juego) {
        juego.avanzarTurno();
    }

    public void accionSobre(Juego juego) {
        juego.avanzarTurno();
        juego.avanzarTurno();
    }

    public String toString() {
        return "SKIP " + color;
    }
}

class CartaWild extends Carta {
    public CartaWild() {
        super(Color.NINGUNO);
    }

    public void accionSobre(Juego juego) {
        juego.avanzarTurno();
    }

    public boolean puedeSerJugadoSobre(Carta carta) { return true;}

    public Carta asignarColor(Color color) {
        this.color = color;
        return this;
    }

    public String toString() { return "WILD";}
}


class CartaWildDraw4 extends Carta {
    public CartaWildDraw4() {
        super(Color.NINGUNO);
    }

    public void accionSobre(Juego juego) {
        juego.avanzarTurno();
        juego.robar(4);
        juego.avanzarTurno();
    }

    public boolean puedeSerJugadoSobre(Carta carta) { return true;}


    public Carta asignarColor(Color color) {
        this.color = color;
        return this;
    }

    public String toString() { return "WILD4";}
}
