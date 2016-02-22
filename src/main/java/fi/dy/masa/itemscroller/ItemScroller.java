package fi.dy.masa.itemscroller;

import org.apache.logging.log4j.Logger;

import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.common.Mod.EventHandler;
import net.minecraftforge.fml.common.Mod.Instance;
import net.minecraftforge.fml.common.event.FMLPreInitializationEvent;

@Mod(modid = Reference.MOD_ID, name = Reference.MOD_NAME, version = Reference.MOD_VERSION,
    clientSideOnly=true, acceptedMinecraftVersions = "1.8,1.8.8,1.8.9")
public class ItemScroller
{
    @Instance(Reference.MOD_ID)
    public static ItemScroller instance;

    public static Logger logger;

    @EventHandler
    public void preInit(FMLPreInitializationEvent event)
    {
        instance = this;
        logger = event.getModLog();
        MinecraftForge.EVENT_BUS.register(new InputEventHandler());
    }
}
