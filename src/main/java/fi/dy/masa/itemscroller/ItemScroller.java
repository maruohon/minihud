package fi.dy.masa.itemscroller;

import org.apache.logging.log4j.Logger;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.common.SidedProxy;
import net.minecraftforge.fml.common.event.FMLPreInitializationEvent;
import fi.dy.masa.itemscroller.config.Configs;
import fi.dy.masa.itemscroller.proxy.CommonProxy;

@Mod(modid = Reference.MOD_ID, name = Reference.MOD_NAME, version = Reference.MOD_VERSION,
    guiFactory = "fi.dy.masa.itemscroller.config.ItemScrollerGuiFactory",
    updateJSON = "https://raw.githubusercontent.com/maruohon/itemscroller/master/update.json",
    clientSideOnly=true, acceptedMinecraftVersions = "[1.11,1.11.2]")
public class ItemScroller
{
    @Mod.Instance(Reference.MOD_ID)
    public static ItemScroller instance;

    @SidedProxy(clientSide = "fi.dy.masa.itemscroller.proxy.ClientProxy", serverSide = "fi.dy.masa.itemscroller.proxy.CommonProxy")
    public static CommonProxy proxy;

    public static Logger logger;

    @Mod.EventHandler
    public void preInit(FMLPreInitializationEvent event)
    {
        logger = event.getModLog();
        proxy.registerEventHandlers();
        Configs.loadConfigsFromFile(event.getSuggestedConfigurationFile());
    }
}
