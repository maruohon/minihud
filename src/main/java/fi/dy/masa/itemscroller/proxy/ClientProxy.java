package fi.dy.masa.itemscroller.proxy;

import org.lwjgl.input.Keyboard;
import net.minecraft.client.settings.KeyBinding;
import net.minecraftforge.client.settings.KeyConflictContext;
import net.minecraftforge.client.settings.KeyModifier;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.fml.client.registry.ClientRegistry;
import fi.dy.masa.itemscroller.config.Configs;
import fi.dy.masa.itemscroller.event.InputEventHandler;

public class ClientProxy extends CommonProxy
{
    public static final KeyBinding KEY_DISABLE = new KeyBinding("itemscroller.desc.toggledisable", KeyConflictContext.GUI, KeyModifier.CONTROL, Keyboard.KEY_S, "itemscroller.category");

    @Override
    public void registerEventHandlers()
    {
        MinecraftForge.EVENT_BUS.register(new InputEventHandler());
        MinecraftForge.EVENT_BUS.register(new Configs());

        ClientRegistry.registerKeyBinding(KEY_DISABLE);
    }
}
