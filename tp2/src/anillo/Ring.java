package anillo;

import java.util.Stack;

abstract class RingNode {
    public abstract Ring add(Stack<RingNode> stack, Object cargo);
    public abstract Ring next(Stack<RingNode> stack);
    public abstract Ring remove(Stack<RingNode> stack);
    public abstract Object current();
    public abstract Ring onEmpty(Stack<RingNode> stack); // hook
}

// Nodo con datos
class DataNode extends RingNode {
    private final Object cargo;

    DataNode(Object cargo) {
        this.cargo = cargo;
    }

    public Ring add(Stack<RingNode> stack, Object newCargo) {
        stack.push(new DataNode(newCargo));
        return new Ring(stack);
    }

    public Ring next(Stack<RingNode> stack) {
        Stack<RingNode> aux = new Stack<>();

        RingNode empty = stack.removeFirst();     // Sacamos el EmptyNode fijo

        RingNode current = stack.pop();       // Top actual
        // Invertimos el resto en aux
        transfer(stack, aux);

        // Insertamos el actual al fondo
        stack.push(current);
        transfer(aux, stack);

        // Reponemos el EmptyNode
        stack.insertElementAt(empty, 0);

        return new Ring(stack);
    }


    private Ring moveTopToBottom(Stack<RingNode> original, Stack<RingNode> aux) {
        RingNode top = original.pop(); // guardamos el actual
        transfer(original, aux);      // movemos todoa aux
        original.push(top);           // ponemos el primero abajo
        transfer(aux, original);      // volvemos
        return new Ring(original);
    }

    private void transfer(Stack<RingNode> from, Stack<RingNode> to) {
        while (!from.empty()) {
            to.push(from.pop());
        }
    }

    public Ring remove(Stack<RingNode> stack) {
        stack.pop();                   // quitamos el actual
        return stack.peek().onEmpty(stack); // delegamos decisión al siguiente nodo
    }

    public Object current() {
        return cargo;
    }

    public Ring onEmpty(Stack<RingNode> stack) {
        return new Ring(stack); // no hacer nada si no está vacío
    }
}

// Nodo vacío
class EmptyNode extends RingNode {
    public Ring add(Stack<RingNode> stack, Object cargo) {
       // stack.pop(); // quitamos el nodo vacío
        //stack.push(new DataNode(cargo));
        stack.push(new DataNode(cargo));  // lo agregás arriba del EmptyNode
        return new Ring(stack);
    }

    public Ring next(Stack<RingNode> stack) {
        throw new RuntimeException("next() no permitido en anillo vacío");
    }

    public Ring remove(Stack<RingNode> stack) {
        throw new RuntimeException("remove() no permitido en anillo vacío");
    }

    public Object current() {
        throw new RuntimeException("current() no permitido en anillo vacío");
    }

    public Ring onEmpty(Stack<RingNode> stack) {
        stack.push(this); // volvemos a poner el nodo vacío
        return new Ring(stack);
    }
}

// Clase principal
public class Ring {
    private final Stack<RingNode> stack;

    public Ring() {
        this.stack = new Stack<>();
        this.stack.push(new EmptyNode());
    }

    public Ring(Stack<RingNode> stack) {
        this.stack = stack;
    }

    public Ring add(Object cargo) {
        return top().add(stack, cargo);
    }

    public Ring next() {
        return top().next(stack); //se agarra el de arriba luego de cambiar orden en stack
    }

    public Ring remove() {
        return top().remove(stack);
    }

    public Object current() {
        return top().current();
    }

    private RingNode top() {
        return stack.peek();
    }
}




