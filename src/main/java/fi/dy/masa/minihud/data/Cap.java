package fi.dy.masa.minihud.data;

public class Cap
{
    protected int current;
    protected int cap;

    public int getCurrent()
    {
        return this.current;
    }

    public int getCap()
    {
        return this.cap;
    }

    public void setCurrent(int current)
    {
        this.current = current;
    }

    public void setCap(int cap)
    {
        this.cap = cap;
    }

    public void setCurrentAndCap(int current, int cap)
    {
        this.current = current;
        this.cap = cap;
    }
}
