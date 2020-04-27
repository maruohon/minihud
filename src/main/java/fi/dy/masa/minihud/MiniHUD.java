package fi.dy.masa.minihud;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import fi.dy.masa.malilib.event.InitializationHandler;
import fi.dy.masa.minihud.config.Configs;
import net.fabricmc.api.ModInitializer;

public class MiniHUD implements ModInitializer
{
    public static final Logger logger = LogManager.getLogger(Reference.MOD_ID);
    public static final String CHANNEL_CARPET_CLIENT = "CarpetClient";

    @Override
    public void onInitialize()
    {
        InitializationHandler.getInstance().registerInitializationHandler(new InitHandler());
    }

    public static void printDebug(String key, Object... args)
    {
        if (Configs.Generic.DEBUG_MESSAGES.getBooleanValue())
        {
            logger.info(key, args);
        }
    }
}
