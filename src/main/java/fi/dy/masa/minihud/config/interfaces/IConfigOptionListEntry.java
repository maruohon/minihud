package fi.dy.masa.minihud.config.interfaces;

public interface IConfigOptionListEntry
{
    String getStringValue();

    String getDisplayName();

    int getOrdinalValue();

    IConfigOptionListEntry cycle(boolean forward);

    IConfigOptionListEntry fromString(String value);
}
