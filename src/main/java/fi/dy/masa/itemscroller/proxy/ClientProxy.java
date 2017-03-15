package fi.dy.masa.itemscroller.proxy;

import org.lwjgl.input.Keyboard;
import net.minecraft.client.settings.KeyBinding;
import net.minecraftforge.client.settings.KeyConflictContext;
import net.minecraftforge.client.settings.KeyModifier;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.fml.client.registry.ClientRegistry;
import fi.dy.masa.itemscroller.config.Configs;
import fi.dy.masa.itemscroller.event.InputEventHandler;
import fi.dy.masa.itemscroller.event.RenderEventHandler;

public class ClientProxy extends CommonProxy
{
    public static final KeyBinding KEY_DISABLE = new KeyBinding("itemscroller.desc.toggledisable", KeyConflictContext.GUI, KeyModifier.CONTROL, Keyboard.KEY_S, "itemscroller.category");
    public static final KeyBinding KEY_RECIPE = new KeyBinding("itemscroller.desc.recipe", KeyConflictContext.GUI, KeyModifier.NONE, Keyboard.KEY_S, "itemscroller.category");

    @Override
    public void registerEventHandlers()
    {
        MinecraftForge.EVENT_BUS.register(new Configs());
        MinecraftForge.EVENT_BUS.register(new InputEventHandler());
        MinecraftForge.EVENT_BUS.register(new RenderEventHandler());

        ClientRegistry.registerKeyBinding(KEY_DISABLE);
        ClientRegistry.registerKeyBinding(KEY_RECIPE);
    }
}
