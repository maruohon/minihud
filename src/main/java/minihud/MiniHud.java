package minihud;

import net.ornithemc.osl.entrypoints.api.client.ClientModInitializer;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import malilib.registry.Registry;
import minihud.config.Configs;

public class MiniHud implements ClientModInitializer
{
    public static final Logger LOGGER = LogManager.getLogger(Reference.MOD_ID);

    public static void debugLog(String msg, Object... args)
    {
        if (Configs.Generic.DEBUG_MESSAGES.getBooleanValue())
        {
            LOGGER.info(msg, args);
        }
    }

    @Override
    public void initClient()
    {
        Registry.INITIALIZATION_DISPATCHER.registerInitializationHandler(new InitHandler());
    }
}
