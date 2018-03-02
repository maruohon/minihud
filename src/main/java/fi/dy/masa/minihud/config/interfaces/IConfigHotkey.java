package fi.dy.masa.minihud.config.interfaces;

public interface IConfigHotkey extends IConfig
{
    int getBitMask();

    String getHotkey();

    void setHotkey(String key);
}
