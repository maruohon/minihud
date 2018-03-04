package fi.dy.masa.minihud.config.interfaces;

import javax.annotation.Nullable;

public interface IConfig
{
    ConfigType getType();

    String getName();

    @Nullable
    String getComment();

    String getStringValue();
}
