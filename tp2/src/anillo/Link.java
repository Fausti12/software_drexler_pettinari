package anillo;

abstract class Link {
    public abstract Link add(Object cargo);
    public abstract Link next();
    public abstract void remove();
    public abstract Object current();
}

class DataLink extends Link {
    private Object cargo;
    private Link next;
    private  Link prev;

    DataLink(Object cargo) {
        this.cargo = cargo;
        this.next = this;
        this.prev = this;
    }

    public Link add(Object cargo) {
        DataLink newNode = new DataLink(cargo);
        newNode.setNext(this);
        newNode.setPrev((DataLink) this.getPrev());
        ((DataLink) this.getPrev()).setNext(newNode);
        this.setPrev(newNode);
        return newNode;
    }

    public Link next() { return next; }

    public void remove(){
        this.cargo = this.next.current();
        this.next = ((DataLink) this.next).getNext();
        this.prev = ((DataLink) this.prev).getPrev();
    }

    public Object current() { return cargo; }

    public Link getNext() { return next; }

    public Link getPrev() { return prev; }

    public void setNext(DataLink next) { this.next = next; }

    public void setPrev(DataLink prev) { this.prev = prev; }

}


class EmptyLink extends Link {

    public Link add(Object cargo) { return new DataLink(cargo); }

    public Link next() {
        throw new RuntimeException("next() not allowed in empty ring");
    }

    public void remove() {
        throw new RuntimeException("remove() not allowed in empty ring");
    }

    public Object current() {
        throw new RuntimeException("current() not allowed in empty ring");
    }

}
