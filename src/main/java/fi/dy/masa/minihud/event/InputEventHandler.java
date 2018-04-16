package fi.dy.masa.minihud.event;

import java.util.EnumSet;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import org.lwjgl.input.Keyboard;
import com.google.common.collect.MapMaker;
import fi.dy.masa.minihud.LiteModMiniHud;
import fi.dy.masa.minihud.config.Configs;
import fi.dy.masa.minihud.config.ConfigsGeneric;
import fi.dy.masa.minihud.config.DebugHotkeys;
import fi.dy.masa.minihud.mixin.IMixinDebugRenderer;
import fi.dy.masa.minihud.mixin.IMixinPathNavigate;
import fi.dy.masa.minihud.util.DebugInfoUtils;
import fi.dy.masa.minihud.util.IMinecraftAccessor;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiScreen;
import net.minecraft.client.renderer.debug.DebugRendererNeighborsUpdate;
import net.minecraft.client.settings.KeyBinding;
import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityLiving;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.network.PacketBuffer;
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
    private static final InputEventHandler INSTANCE = new InputEventHandler();

    private final Set<Integer> modifierKeys = new HashSet<>();
    private boolean neighborUpdateEnabled;
    private boolean pathfindingEnabled;
    private boolean toggledInfo;
    private int tickCounter;
    private final Map<Entity, Path> oldPaths = new MapMaker().weakKeys().weakValues().<Entity, Path>makeMap();

    private InputEventHandler()
    {
        this.modifierKeys.add(Keyboard.KEY_LSHIFT);
        this.modifierKeys.add(Keyboard.KEY_RSHIFT);
        this.modifierKeys.add(Keyboard.KEY_LCONTROL);
        this.modifierKeys.add(Keyboard.KEY_RCONTROL);
        this.modifierKeys.add(Keyboard.KEY_LMENU);
        this.modifierKeys.add(Keyboard.KEY_RMENU);
    }

    public static InputEventHandler getInstance()
    {
        return INSTANCE;
    }

    public boolean onKeyInput()
    {
        Minecraft mc = Minecraft.getMinecraft();

        // Keybinds shouldn't work inside GUIs
        if (mc.currentScreen != null)
        {
            return false;
        }

        int eventKey = Keyboard.getEventKey();
        boolean eventKeyState = Keyboard.getEventKeyState();
        int bitMaskForEventKey = Configs.getBitmaskForDebugKey(eventKey);
        boolean cancel = false;

        if (eventKeyState && Keyboard.isKeyDown(Keyboard.KEY_F3) && bitMaskForEventKey != 0)
        {
            this.toggleDebugRenderers(mc, bitMaskForEventKey);
            KeyBinding.setKeyBindState(eventKey, false);

            // This prevent the F3 screen from opening after releasing the F3 key
            ((IMinecraftAccessor) mc).setActionKeyF3(true);
            cancel = true;
        }

        int toggleKey = LiteModMiniHud.KEY_TOGGLE_MODE.getKeyCode();

        // Toggle the HUD when releasing the toggle key, if no info types were toggled while it was down
        if (eventKeyState == false && eventKey == toggleKey)
        {
            if (this.toggledInfo == false)
            {
                RenderEventHandler.getInstance().toggleEnabled();
            }

            this.toggledInfo = false;
            cancel = true;
        }
        else if (eventKeyState && Keyboard.isKeyDown(toggleKey))
        {
            bitMaskForEventKey = Configs.getBitmaskForInfoKey(eventKey);

            if (bitMaskForEventKey != 0)
            {
                RenderEventHandler.getInstance().xorInfoLineEnabledMask(bitMaskForEventKey);
                this.toggledInfo = true;
                cancel = true;
            }

            bitMaskForEventKey = Configs.getBitmaskForOverlayKey(eventKey);

            if (bitMaskForEventKey != 0)
            {
                RenderEventHandler.getInstance().xorOverlayRendererEnabledMask(bitMaskForEventKey);
                this.toggledInfo = true;
                cancel = true;
            }
        }

        return cancel && this.modifierKeys.contains(eventKey) == false;
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
                    for (EnumFacing side : notifiedSides)
                    {
                        ((DebugRendererNeighborsUpdate) Minecraft.getMinecraft().debugRenderer.neighborsUpdate).addUpdate(time, pos.offset(side));
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

                    if (navigator != null && this.isAnyPlayerWithinRange(world, entity, 64))
                    {
                        final Path path = navigator.getPath();
                        Path old = this.oldPaths.get(entity);

                        if (path == null)
                        {
                            continue;
                        }

                        boolean isSamepath = old != null && old.isSamePath(path);

                        if (old == null || isSamepath == false || old.getCurrentPathIndex() != path.getCurrentPathIndex())
                        {
                            final int id = entity.getEntityId();
                            final float maxDistance = ConfigsGeneric.DEBUG_RENDERER_PATH_MAX_DIST.getBooleanValue() ? ((IMixinPathNavigate) navigator).getMaxDistanceToWaypoint() : 0F;

                            DebugInfoUtils.sendPacketDebugPath(server, id, path, maxDistance);

                            if (isSamepath == false)
                            {
                                // Make a copy via a PacketBuffer... :/
                                PacketBuffer buf = DebugInfoUtils.writePathTobuffer(path);
                                this.oldPaths.put(entity, Path.read(buf));
                            }
                            else if (old != null)
                            {
                                old.setCurrentPathIndex(path.getCurrentPathIndex());
                            }
                        }
                    }
                }
            }
        }
    }

    private boolean isAnyPlayerWithinRange(World world, Entity entity, double range)
    {
        for (int i = 0; i < world.playerEntities.size(); ++i)
        {
            EntityPlayer player = world.playerEntities.get(i);

            double distSq = player.getDistanceSq(entity.posX, entity.posY, entity.posZ);

            if (range < 0.0D || distSq < range * range)
            {
                return true;
            }
        }

        return false;
    }

    private void toggleDebugRenderers(Minecraft mc, int mask)
    {
        for (int i = 0; i < 6; i++)
        {
            int bit = mask & (1 << i);
            boolean status;

            if (bit == DebugHotkeys.COLLISION_BOXES.getBitMask())
            {
                status = ! ((IMixinDebugRenderer) mc.debugRenderer).getCollisionBoxEnabled();
                ((IMixinDebugRenderer) mc.debugRenderer).setCollisionBoxEnabled(status);
                this.printMessage(mc, "collisions", status ? "ON" : "OFF");
            }
            else if (bit == DebugHotkeys.HEIGHT_MAP.getBitMask())
            {
                status = ! ((IMixinDebugRenderer) mc.debugRenderer).getHeightMapEnabled();
                ((IMixinDebugRenderer) mc.debugRenderer).setHeightMapEnabled(status);
                this.printMessage(mc, "height_map", status ? "ON" : "OFF");
            }
            else if (bit == DebugHotkeys.NEIGHBOR_UPDATES.getBitMask())
            {
                status = ! ((IMixinDebugRenderer) mc.debugRenderer).getNeighborsUpdateEnabled();
                ((IMixinDebugRenderer) mc.debugRenderer).setNeighborsUpdateEnabled(status);
                this.neighborUpdateEnabled = status;
                this.printMessage(mc, "neighbor_updates", status ? "ON" : "OFF");
            }
            else if (bit == DebugHotkeys.PATH_FINDING.getBitMask())
            {
                status = ! ((IMixinDebugRenderer) mc.debugRenderer).getPathfindingEnabled();
                ((IMixinDebugRenderer) mc.debugRenderer).setPathfindingEnabled(status);
                this.pathfindingEnabled = status;
                this.printMessage(mc, "pathfinding", status ? "ON" : "OFF");
            }
            else if (bit == DebugHotkeys.SOLID_FACES.getBitMask())
            {
                status = ! ((IMixinDebugRenderer) mc.debugRenderer).getSolidFaceEnabled();
                ((IMixinDebugRenderer) mc.debugRenderer).setSolidFaceEnabled(status);
                this.printMessage(mc, "solid_faces", status ? "ON" : "OFF");
            }
            else if (bit == DebugHotkeys.WATER.getBitMask())
            {
                status = ! ((IMixinDebugRenderer) mc.debugRenderer).getWaterEnabled();
                ((IMixinDebugRenderer) mc.debugRenderer).setWaterEnabled(status);
                this.printMessage(mc, "water", status ? "ON" : "OFF");
            }
        }
    }

    private void printMessage(Minecraft mc, String key, Object... args)
    {
        mc.ingameGUI.addChatMessage(ChatType.GAME_INFO, new TextComponentTranslation("minihud.message.toggled_debug_mode." + key, args));
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
