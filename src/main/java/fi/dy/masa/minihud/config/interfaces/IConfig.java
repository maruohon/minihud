package fi.dy.masa.minihud.config.interfaces;

import javax.annotation.Nullable;
import fi.dy.masa.minihud.config.Configs.ConfigType;

public interface IConfig
{
    ConfigType getType();

    String getName();

    @Nullable
    String getComment();

    String getStringValue();
}
