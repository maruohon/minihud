package fi.dy.masa.minihud.event;

import java.lang.reflect.Field;
import java.util.EnumSet;
import java.util.Map;
import org.lwjgl.input.Keyboard;
import com.google.common.collect.MapMaker;
import fi.dy.masa.minihud.LiteModMiniHud;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.util.DebugInfoUtils;
import fi.dy.masa.minihud.util.ReflectionHelper;
import fi.dy.masa.minihud.util.ReflectionHelper.UnableToFindFieldException;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiScreen;
import net.minecraft.client.renderer.debug.DebugRenderer;
import net.minecraft.client.renderer.debug.DebugRendererNeighborsUpdate;
import net.minecraft.client.settings.KeyBinding;
import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityLiving;
import net.minecraft.pathfinding.Path;
import net.minecraft.pathfinding.PathNavigate;
import net.minecraft.server.MinecraftServer;
import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.text.ChatType;
import net.minecraft.util.text.TextComponentTranslation;
import net.minecraft.world.World;

public class InputEventHandler
{
    public static final int MASK_DEBUG_COLLISION_BOXES  = 0x01;
    public static final int MASK_DEBUG_HEIGHT_MAP       = 0x02;
    public static final int MASK_DEBUG_NEIGHBOR_UPDATE  = 0x04;
    public static final int MASK_DEBUG_PATHFINDING      = 0x08;
    public static final int MASK_DEBUG_SOLID_FACES      = 0x10;
    public static final int MASK_DEBUG_WATER            = 0x20;

    private static final InputEventHandler INSTANCE = new InputEventHandler();

    private boolean toggledInfo;

    private Field field_Minecraft_actionKeyF3;
    private Field field_DebugRenderer_collisionBoxEnabled;
    private Field field_DebugRenderer_heightMapEnabled;
    private Field field_DebugRenderer_neighborsUpdateEnabled;
    private Field field_DebugRenderer_pathfindingEnabled;
    private Field field_DebugRenderer_solidFaceEnabled;
    private Field field_DebugRenderer_waterEnabled;
    private boolean neighborUpdateEnabled;
    private boolean pathfindingEnabled;
    private int tickCounter;
    private final Map<Entity, Path> oldPaths = new MapMaker().weakKeys().weakValues().<Entity, Path>makeMap();

    public InputEventHandler()
    {
        try
        {
            this.field_Minecraft_actionKeyF3                = ReflectionHelper.findField(Minecraft.class, "field_184129_aV", "actionKeyF3");
            this.field_DebugRenderer_collisionBoxEnabled    = ReflectionHelper.findField(DebugRenderer.class, "field_191326_j", "collisionBoxEnabled");
            this.field_DebugRenderer_heightMapEnabled       = ReflectionHelper.findField(DebugRenderer.class, "field_190082_h", "heightMapEnabled");
            this.field_DebugRenderer_neighborsUpdateEnabled = ReflectionHelper.findField(DebugRenderer.class, "field_191558_l", "neighborsUpdateEnabled");
            this.field_DebugRenderer_pathfindingEnabled     = ReflectionHelper.findField(DebugRenderer.class, "field_190080_f", "pathfindingEnabled");
            this.field_DebugRenderer_solidFaceEnabled       = ReflectionHelper.findField(DebugRenderer.class, "field_193853_n", "solidFaceEnabled");
            this.field_DebugRenderer_waterEnabled           = ReflectionHelper.findField(DebugRenderer.class, "field_190081_g", "waterEnabled");
        }
        catch (UnableToFindFieldException e)
        {
            LiteModMiniHud.logger.warn("Failed to reflect DebugRenderer fields");
        }
    }

    public static InputEventHandler getInstance()
    {
        return INSTANCE;
    }

    public boolean onKeyInput()
    {
        Minecraft mc = Minecraft.getMinecraft();
        int eventKey = Keyboard.getEventKey();
        boolean eventKeyState = Keyboard.getEventKeyState();
        int bitMaskForEventKey = Configs.getBitmaskForDebugKey(eventKey);
        boolean cancel = false;

        if (eventKeyState && Keyboard.isKeyDown(Keyboard.KEY_F3) && bitMaskForEventKey != 0)
        {
            this.toggleDebugRenderers(mc, bitMaskForEventKey);
            KeyBinding.setKeyBindState(eventKey, false);

            // This prevent the F3 screen from opening after releasing the F3 key
            this.setBoolean(this.field_Minecraft_actionKeyF3, mc, true);
            cancel = true;
        }

        int toggleKey = LiteModMiniHud.KEY_TOGGLE_MODE.getKeyCode();
        bitMaskForEventKey = Configs.getBitmaskForInfoKey(eventKey);

        // Toggle the HUD when releasing the toggle key, if no infos were toggled while it was down
        if (eventKeyState == false && eventKey == toggleKey)
        {
            if (this.toggledInfo == false)
            {
                RenderEventHandler.getInstance().toggleEnabled();
                cancel = true;
            }

            this.toggledInfo = false;
        }
        else if (eventKeyState && bitMaskForEventKey != 0 && Keyboard.isKeyDown(toggleKey))
        {
            RenderEventHandler.getInstance().xorEnabledMask(bitMaskForEventKey);
            this.toggledInfo = true;
            KeyBinding.unPressAllKeys();
            cancel = true;
        }

        return cancel;
    }

    public void onNeighborNotify(World world, BlockPos pos, EnumSet<EnumFacing> notifiedSides)
    {
        // This will only work in single player...
        // We are catching updates from the server world, and adding them to the debug renderer directly
        if (this.neighborUpdateEnabled && world.isRemote == false)
        {
            final long time = world.getTotalWorldTime();

            Minecraft.getMinecraft().addScheduledTask(new Runnable()
            {
                public void run()
                {
                    // field_191557_f = neighborsUpdate
                    // func_191553_a() = addUpdate()
                    for (EnumFacing side : notifiedSides)
                    {
                        ((DebugRendererNeighborsUpdate) Minecraft.getMinecraft().debugRenderer.field_191557_f).func_191553_a(time, pos.offset(side));
                    }
                }
            });
        }
    }

    public void onServerTickEnd(MinecraftServer server)
    {
        Minecraft mc = Minecraft.getMinecraft();

        // Send the custom packet with the Path data, if that debug renderer is enabled
        if (this.pathfindingEnabled && mc.world != null && ++this.tickCounter >= 10)
        {
            this.tickCounter = 0;
            World world = server.getWorld(mc.world.provider.getDimensionType().getId());

            if (world != null)
            {
                for (Entity entity : world.loadedEntityList)
                {
                    PathNavigate navigator = entity instanceof EntityLiving ? ((EntityLiving) entity).getNavigator() : null;

                    if (navigator != null)
                    {
                        final Path path = navigator.getPath();
                        Path old = this.oldPaths.get(entity);

                        if (path != null && (old == null || old.isSamePath(path) == false))
                        {
                            final int id = entity.getEntityId();
                            final float maxDistance = Configs.Generic.DEBUG_RENDERER_PATH_MAX_DIST.getBooleanValue() ? navigator.getPathSearchRange() : 0F;
                            this.oldPaths.put(entity, path);

                            DebugInfoUtils.sendPacketDebugPath(server, id, path, maxDistance);
                        }
                    }
                }
            }
        }
    }

    private void toggleDebugRenderers(Minecraft mc, int mask)
    {
        for (int i = 0; i < 6; i++)
        {
            int bit = mask & (1 << i);
            boolean status;

            switch (bit)
            {
                case MASK_DEBUG_COLLISION_BOXES:
                    status = this.toggleBoolean(this.field_DebugRenderer_collisionBoxEnabled, mc.debugRenderer);
                    this.printMessage(mc, "collisions", status ? "ON" : "OFF");
                    break;

                case MASK_DEBUG_HEIGHT_MAP:
                    status = this.toggleBoolean(this.field_DebugRenderer_heightMapEnabled, mc.debugRenderer);
                    this.printMessage(mc, "height_map", status ? "ON" : "OFF");
                    break;

                case MASK_DEBUG_NEIGHBOR_UPDATE:
                    status = this.toggleBoolean(this.field_DebugRenderer_neighborsUpdateEnabled, mc.debugRenderer);
                    this.neighborUpdateEnabled = status;
                    this.printMessage(mc, "neighbor_updates", status ? "ON" : "OFF");
                    break;

                case MASK_DEBUG_PATHFINDING:
                    status = this.toggleBoolean(this.field_DebugRenderer_pathfindingEnabled, mc.debugRenderer);
                    this.pathfindingEnabled = status;
                    this.printMessage(mc, "pathfinding", status ? "ON" : "OFF");
                    break;

                case MASK_DEBUG_SOLID_FACES:
                    status = this.toggleBoolean(this.field_DebugRenderer_solidFaceEnabled, mc.debugRenderer);
                    this.printMessage(mc, "solid_faces", status ? "ON" : "OFF");
                    break;

                case MASK_DEBUG_WATER:
                    status = this.toggleBoolean(this.field_DebugRenderer_waterEnabled, mc.debugRenderer);
                    this.printMessage(mc, "water", status ? "ON" : "OFF");
                    break;

                default:
                    break;
            }
        }
    }

    private void printMessage(Minecraft mc, String key, Object... args)
    {
        // func_191742_a() = addChatMessage()
        mc.ingameGUI.func_191742_a(ChatType.GAME_INFO, new TextComponentTranslation("minihud.message.toggled_debug_mode." + key, args));
    }

    private void setBoolean(Field field, Object obj, boolean value)
    {
        try
        {
            field.set(obj, value);
        }
        catch (Exception e)
        {
            LiteModMiniHud.logger.warn("InputEventHandler: Failed set a boolean on a reflected field");
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
            LiteModMiniHud.logger.warn("InputEventHandler: Failed to toggle a boolean on a reflected field");
        }

        return false;
    }

    public static boolean isRequiredKeyActive()
    {
        KeyModifier key = Configs.requiredKey;

        if (key == KeyModifier.NONE)    { return true;                       }
        if (key == KeyModifier.ALT)     { return GuiScreen.isAltKeyDown();   }
        if (key == KeyModifier.CONTROL) { return GuiScreen.isCtrlKeyDown();  }
        if (key == KeyModifier.SHIFT)   { return GuiScreen.isShiftKeyDown(); }
        return false;
    }

    public enum KeyModifier
    {
        NONE,
        ALT,
        CONTROL,
        SHIFT;
    }
}
