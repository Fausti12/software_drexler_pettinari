package uno;

abstract class Controlador {
    abstract Jugador jugadorSiguiente(Jugador player);
    abstract Controlador opuesto();
}

class ControladorDerecha extends Controlador{
    Jugador jugadorSiguiente(Jugador player) {
        return player.getDerecha();
    }
    Controlador opuesto() {
        return new ControladorIzquierda();
    }
}

class ControladorIzquierda extends Controlador{
    Jugador jugadorSiguiente(Jugador player) {
        return player.getIzquierda();
    }
    Controlador opuesto() {
        return new ControladorDerecha();
    }
}
