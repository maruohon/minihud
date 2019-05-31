package fi.dy.masa.itemscroller;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.dimdev.rift.listener.client.ClientTickable;
import org.dimdev.riftloader.listener.InitializationListener;
import org.spongepowered.asm.launch.MixinBootstrap;
import org.spongepowered.asm.mixin.Mixins;
import fi.dy.masa.itemscroller.event.KeybindCallbacks;
import fi.dy.masa.malilib.event.InitializationHandler;
import net.minecraft.client.Minecraft;

public class ItemScroller implements ClientTickable, InitializationListener
{
    public static final Logger logger = LogManager.getLogger(Reference.MOD_ID);

    @Override
    public void onInitialization()
    {
        MixinBootstrap.init();
        Mixins.addConfiguration("mixins.itemscroller.json");

        InitializationHandler.getInstance().registerInitializationHandler(new InitHandler());
    }

    @Override
    public void clientTick(Minecraft mc)
    {
        KeybindCallbacks.getInstance().onTick(mc);
    }
}
