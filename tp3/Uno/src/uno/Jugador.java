package uno;

import java.util.ArrayList;
import java.util.List;

public class Jugador {
    private final String nombre;
    private final List<Carta> mano = new ArrayList<>();

    public Jugador(String nombre) {
        this.nombre = nombre;
    }

    public void recibir(Carta carta) {
        mano.add(carta);
    }

    public void jugar(Carta carta) {
        if (!mano.contains(carta)) throw new IllegalArgumentException("La carta no está en la mano");
        mano.remove(carta);
    }

    public boolean tieneCartaJugable(Carta pozo) {
        return mano.stream().anyMatch(carta -> carta.puedeSerJugadoSobre(pozo));
    }

    public boolean tieneUnaCarta() { return mano.size() == 1; }

    public int cantidad() {
        return mano.size();
    }
}
