package fi.dy.masa.minihud;

import org.apache.logging.log4j.Logger;

import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.common.Mod.EventHandler;
import net.minecraftforge.fml.common.Mod.Instance;
import net.minecraftforge.fml.common.SidedProxy;
import net.minecraftforge.fml.common.event.FMLPreInitializationEvent;

import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.proxy.CommonProxy;

@Mod(modid = Reference.MOD_ID, name = Reference.MOD_NAME, version = Reference.MOD_VERSION,
    guiFactory = "fi.dy.masa.minihud.config.MiniHudGuiFactory",
    updateJSON = "https://raw.githubusercontent.com/maruohon/minihud/master/update.json",
    clientSideOnly=true, acceptedMinecraftVersions = "1.9")
public class MiniHud
{
    @Instance(Reference.MOD_ID)
    public static MiniHud instance;

    @SidedProxy(clientSide = "fi.dy.masa.minihud.proxy.ClientProxy", serverSide = "fi.dy.masa.minihud.proxy.CommonProxy")
    public static CommonProxy proxy;

    public static Logger logger;

    @EventHandler
    public void preInit(FMLPreInitializationEvent event)
    {
        instance = this;
        logger = event.getModLog();
        Configs.loadConfigsFromFile(event.getSuggestedConfigurationFile());
        proxy.registerKeyBindings();
        proxy.registerEventHandlers();
    }
}
