package fi.dy.masa.minihud.util;

public interface ConduitExtra
{
    int getCurrentActivatingBlockCount();

    int getStoredActivatingBlockCount();

    void setActivatingBlockCount(int count);
}
