package fi.dy.masa.minihud.event;

import java.lang.reflect.Field;
import org.lwjgl.input.Keyboard;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiScreen;
import net.minecraft.client.renderer.debug.DebugRenderer;
import net.minecraft.client.renderer.debug.DebugRendererNeighborsUpdate;
import net.minecraft.client.settings.KeyBinding;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.text.TextComponentTranslation;
import net.minecraftforge.client.settings.KeyModifier;
import net.minecraftforge.event.world.BlockEvent.NeighborNotifyEvent;
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent;
import net.minecraftforge.fml.common.gameevent.InputEvent.KeyInputEvent;
import net.minecraftforge.fml.relauncher.ReflectionHelper;
import net.minecraftforge.fml.relauncher.ReflectionHelper.UnableToFindFieldException;
import net.minecraftforge.fml.relauncher.Side;
import net.minecraftforge.fml.relauncher.SideOnly;
import fi.dy.masa.minihud.MiniHud;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.proxy.ClientProxy;

@SideOnly(Side.CLIENT)
public class InputEventHandler
{
    public static final int MASK_DEBUG_COLLISION_BOXES  = 0x01;
    public static final int MASK_DEBUG_HEIGHT_MAP       = 0x02;
    public static final int MASK_DEBUG_NEIGHBOR_UPDATE  = 0x04;
    public static final int MASK_DEBUG_PATHFINDING      = 0x08;
    public static final int MASK_DEBUG_WATER            = 0x20;

    private final Minecraft mc;
    private boolean toggledInfo;

    private Field field_Minecraft_actionKeyF3;
    private Field field_DebugRenderer_collisionBoxEnabled;
    private Field field_DebugRenderer_heightMapEnabled;
    private Field field_DebugRenderer_neighborsUpdateEnabled;
    private Field field_DebugRenderer_pathfindingEnabled;
    private Field field_DebugRenderer_waterEnabled;
    private boolean neighborUpdateEnabled;

    public InputEventHandler()
    {
        this.mc = Minecraft.getMinecraft();

        try
        {
            this.field_Minecraft_actionKeyF3                = ReflectionHelper.findField(Minecraft.class, "field_184129_aV", "actionKeyF3");
            this.field_DebugRenderer_collisionBoxEnabled    = ReflectionHelper.findField(DebugRenderer.class, "field_191326_j", "collisionBoxEnabled");
            this.field_DebugRenderer_heightMapEnabled       = ReflectionHelper.findField(DebugRenderer.class, "field_190082_h", "heightMapEnabled");
            this.field_DebugRenderer_neighborsUpdateEnabled = ReflectionHelper.findField(DebugRenderer.class, "field_191558_l", "neighborsUpdateEnabled");
            this.field_DebugRenderer_pathfindingEnabled     = ReflectionHelper.findField(DebugRenderer.class, "field_190080_f", "pathfindingEnabled");
            this.field_DebugRenderer_waterEnabled           = ReflectionHelper.findField(DebugRenderer.class, "field_190081_g", "waterEnabled");
        }
        catch (UnableToFindFieldException e)
        {
            MiniHud.logger.warn("Failed to reflect DebugRenderer fields");
        }
    }

    @SubscribeEvent
    public void onKeyInputEvent(KeyInputEvent event)
    {
        int key = Keyboard.getEventKey();
        boolean state = Keyboard.getEventKeyState();
        int mask = Configs.getBitmaskForDebugKey(key);

        if (state && Keyboard.isKeyDown(Keyboard.KEY_F3) && mask != 0)
        {
            this.toggleDebugRenderers(mask);
            KeyBinding.setKeyBindState(key, false);

            // This prevent the F3 screen from opening after releasing the F3 key
            this.setBoolean(this.field_Minecraft_actionKeyF3, this.mc, true);

            return;
        }

        int toggleKey = ClientProxy.keyToggleMode.getKeyCode();
        mask = Configs.getBitmaskForInfoKey(key);

        // Toggle the HUD when releasing the toggle key, if no infos were toggled while it was down
        if (state == false && key == toggleKey)
        {
            if (this.toggledInfo == false)
            {
                RenderEventHandler.getInstance().toggleEnabled();
            }

            this.toggledInfo = false;
        }
        else if (state && mask != 0 && Keyboard.isKeyDown(toggleKey))
        {
            RenderEventHandler.getInstance().xorEnabledMask(mask);
            this.toggledInfo = true;
            KeyBinding.unPressAllKeys();
        }
    }

    @SubscribeEvent
    public void onNeighborNotify(NeighborNotifyEvent event)
    {
        // This will only work in single player...
        // We are catching updates from the server world, and adding them to the debug renderer directly
        if (this.neighborUpdateEnabled && event.getWorld().isRemote == false)
        {
            final long time = event.getWorld().getTotalWorldTime();
            final BlockPos pos = event.getPos();

            this.mc.addScheduledTask(new Runnable()
            {
                public void run()
                {
                    ((DebugRendererNeighborsUpdate) Minecraft.getMinecraft().debugRenderer.field_191557_f).func_191553_a(time, pos);
                }
            });
        }
    }

    private void toggleDebugRenderers(int mask)
    {
        for (int i = 0; i < 6; i++)
        {
            int bit = mask & (1 << i);
            boolean status;

            switch (bit)
            {
                case MASK_DEBUG_COLLISION_BOXES:
                    status = this.toggleBoolean(this.field_DebugRenderer_collisionBoxEnabled, this.mc.debugRenderer);
                    this.printMessage("collisions", status ? "ON" : "OFF");
                    break;

                case MASK_DEBUG_HEIGHT_MAP:
                    status = this.toggleBoolean(this.field_DebugRenderer_heightMapEnabled, this.mc.debugRenderer);
                    this.printMessage("height_map", status ? "ON" : "OFF");
                    break;

                case MASK_DEBUG_NEIGHBOR_UPDATE:
                    status = this.toggleBoolean(this.field_DebugRenderer_neighborsUpdateEnabled, this.mc.debugRenderer);
                    this.neighborUpdateEnabled = status;
                    this.printMessage("neighbor_updates", status ? "ON" : "OFF");
                    break;

                case MASK_DEBUG_PATHFINDING:
                    status = this.toggleBoolean(this.field_DebugRenderer_pathfindingEnabled, this.mc.debugRenderer);
                    this.printMessage("pathfinding", status ? "ON" : "OFF");
                    break;

                case MASK_DEBUG_WATER:
                    status = this.toggleBoolean(this.field_DebugRenderer_waterEnabled, this.mc.debugRenderer);
                    this.printMessage("water", status ? "ON" : "OFF");
                    break;

                default:
                    break;
            }
        }
    }

    private void printMessage(String key, Object... args)
    {
        this.mc.ingameGUI.setOverlayMessage(new TextComponentTranslation("minihud.message.toggled_debug_mode." + key, args), false);
    }

    private void setBoolean(Field field, Object obj, boolean value)
    {
        try
        {
            field.set(obj, value);
        }
        catch (Exception e)
        {
            MiniHud.logger.warn("InputEventHandler: Failed set a boolean via a reflected field");
        }
    }

    private boolean toggleBoolean(Field field, Object obj)
    {
        try
        {
            boolean newValue = ! field.getBoolean(obj);
            field.set(obj, newValue);
            return newValue;
        }
        catch (Exception e)
        {
            MiniHud.logger.warn("InputEventHandler: Failed to toggle a boolean via a reflected field");
        }

        return false;
    }

    public static boolean isRequiredKeyActive(KeyModifier key)
    {
        if (key == KeyModifier.NONE)    { return true;                       }
        if (key == KeyModifier.ALT)     { return GuiScreen.isAltKeyDown();   }
        if (key == KeyModifier.CONTROL) { return GuiScreen.isCtrlKeyDown();  }
        if (key == KeyModifier.SHIFT)   { return GuiScreen.isShiftKeyDown(); }
        return false;
    }
}
