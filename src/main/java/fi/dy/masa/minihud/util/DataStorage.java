package fi.dy.masa.minihud.util;

import java.util.regex.Matcher;
import java.util.regex.Pattern;
import fi.dy.masa.minihud.MiniHUD;
import fi.dy.masa.minihud.renderer.OverlayRendererSpawnableColumnHeights;
import net.minecraft.ChatFormat;
import net.minecraft.client.MinecraftClient;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.server.MinecraftServer;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.MathHelper;
import net.minecraft.world.World;
import net.minecraft.world.dimension.DimensionType;

public class DataStorage
{
    private static final Pattern PATTERN_CARPET_TPS = Pattern.compile("TPS: (?<tps>[0-9]+[\\.,][0-9]) MSPT: (?<mspt>[0-9]+[\\.,][0-9])");
    private static final DataStorage INSTANCE = new DataStorage();

    private boolean worldSeedValid;
    private boolean serverTPSValid;
    private boolean carpetServer;
    private boolean worldSpawnValid;
    private long worldSeed;
    private long lastServerTick;
    private long lastServerTimeUpdate;
    private double serverTPS;
    private double serverMSPT;
    private BlockPos worldSpawn = BlockPos.ORIGIN;
    private final MinecraftClient mc = MinecraftClient.getInstance();

    public static DataStorage getInstance()
    {
        return INSTANCE;
    }

    public void reset()
    {
        this.worldSeedValid = false;
        this.serverTPSValid = false;
        this.carpetServer = false;
        this.worldSpawnValid = false;
    }

    public void setWorldSeed(long seed)
    {
        this.worldSeed = seed;
        this.worldSeedValid = true;
    }

    public void setWorldSpawn(BlockPos spawn)
    {
        this.worldSpawn = spawn;
        this.worldSpawnValid = true;
    }

    public boolean isWorldSeedKnown(DimensionType dimension)
    {
        if (this.worldSeedValid)
        {
            return true;
        }
        else if (this.mc.isIntegratedServerRunning())
        {
            MinecraftServer server = this.mc.getServer();
            World worldTmp = server.getWorld(dimension);
            return worldTmp != null;
        }

        return false;
    }

    public long getWorldSeed(DimensionType dimension)
    {
        if (this.worldSeedValid)
        {
            return this.worldSeed;
        }
        else if (this.mc.isIntegratedServerRunning())
        {
            MinecraftServer server = this.mc.getServer();
            World worldTmp = server.getWorld(dimension);
            return worldTmp != null ? worldTmp.getSeed() : 0;
        }

        return 0;
    }

    public boolean isWorldSpawnKnown()
    {
        return this.worldSpawnValid;
    }

    public BlockPos getWorldSpawn()
    {
        return this.worldSpawn;
    }

    public boolean isServerTPSValid()
    {
        return this.serverTPSValid;
    }

    public boolean isCarpetServer()
    {
        return this.carpetServer;
    }

    public double getServerTPS()
    {
        return this.serverTPS;
    }

    public double getServerMSPT()
    {
        return this.serverMSPT;
    }

    public void markChunkForHeightmapCheck(int chunkX, int chunkZ)
    {
        OverlayRendererSpawnableColumnHeights.markChunkChanged(chunkX, chunkZ);
    }

    public boolean onSendChatMessage(PlayerEntity player, String message)
    {
        String[] parts = message.split(" ");

        if (parts[0].equals("minihud-seed"))
        {
            if (parts.length == 2)
            {
                try
                {
                    long seed = Long.parseLong(parts[1]);
                    this.worldSeed = seed;
                    this.worldSeedValid = true;
                    MiscUtils.printInfoMessage("minihud.message.seed_set", Long.valueOf(seed));
                }
                catch (NumberFormatException e)
                {
                    MiscUtils.printInfoMessage("minihud.message.error.invalid_seed");
                }
            }
            else if (this.worldSeedValid && parts.length == 1)
            {
                MiscUtils.printInfoMessage("minihud.message.seed_set", Long.valueOf(this.worldSeed));
            }

            return true;
        }

        return false;
    }

    public void onChatMessage(Component message)
    {
        if (message instanceof TranslatableComponent)
        {
            TranslatableComponent text = (TranslatableComponent) message;

            // The vanilla "/seed" command
            if ("commands.seed.success".equals(text.getKey()))
            {
                try
                {
                    String str = text.getString();
                    int i1 = str.indexOf("[");
                    int i2 = str.indexOf("]");

                    if (i1 != -1 && i2 != -1)
                    {
                        this.worldSeed = Long.parseLong(str.substring(i1 + 1, i2));
                        this.worldSeedValid = true;
                        MiniHUD.logger.info("Received world seed from the vanilla /seed command: {}", this.worldSeed);
                        MiscUtils.printInfoMessage("minihud.message.seed_set", Long.valueOf(this.worldSeed));
                    }
                }
                catch (Exception e)
                {
                    MiniHUD.logger.warn("Failed to read the world seed from '{}'", text.getParams()[0], e);
                }
            }
            // The "/jed seed" command
            else if ("jed.commands.seed.success".equals(text.getKey()))
            {
                try
                {
                    this.worldSeed = Long.parseLong(text.getParams()[1].toString());
                    this.worldSeedValid = true;
                    MiniHUD.logger.info("Received world seed from the JED '/jed seed' command: {}", this.worldSeed);
                    MiscUtils.printInfoMessage("minihud.message.seed_set", Long.valueOf(this.worldSeed));
                }
                catch (Exception e)
                {
                    MiniHUD.logger.warn("Failed to read the world seed from '{}'", text.getParams()[1], e);
                }
            }
            else if ("commands.setworldspawn.success".equals(text.getKey()) && text.getParams().length == 3)
            {
                try
                {
                    Object[] o = text.getParams();
                    int x = Integer.parseInt(o[0].toString());
                    int y = Integer.parseInt(o[1].toString());
                    int z = Integer.parseInt(o[2].toString());

                    this.worldSpawn = new BlockPos(x, y, z);
                    this.worldSpawnValid = true;

                    String spawnStr = String.format("x: %d, y: %d, z: %d", this.worldSpawn.getX(), this.worldSpawn.getY(), this.worldSpawn.getZ());
                    MiniHUD.logger.info("Received world spawn from the vanilla /setworldspawn command: {}", spawnStr);
                    MiscUtils.printInfoMessage("minihud.message.spawn_set", spawnStr);
                }
                catch (Exception e)
                {
                    MiniHUD.logger.warn("Failed to read the world spawn point from '{}'", text.getParams(), e);
                }
            }
        }
    }

    public void onServerTimeUpdate(long totalWorldTime)
    {
        // Carpet server sends the TPS and MSPT values via the player list footer data,
        // and for single player the data is grabbed directly from the integrated server.
        if (this.carpetServer == false && this.mc.isInSingleplayer() == false)
        {
            long currentTime = System.nanoTime();

            if (this.serverTPSValid)
            {
                long elapsedTicks = totalWorldTime - this.lastServerTick;

                if (elapsedTicks > 0)
                {
                    this.serverMSPT = ((double) (currentTime - this.lastServerTimeUpdate) / (double) elapsedTicks) / 1000000D;
                    this.serverTPS = this.serverMSPT <= 50 ? 20D : (1000D / this.serverMSPT);
                }
            }

            this.lastServerTick = totalWorldTime;
            this.lastServerTimeUpdate = currentTime;
            this.serverTPSValid = true;
        }
    }

    public void updateIntegratedServerTPS()
    {
        if (this.mc != null && this.mc.player != null && this.mc.getServer() != null)
        {
            this.serverMSPT = (double) MathHelper.average(this.mc.getServer().lastTickLengths) / 1000000D;
            this.serverTPS = this.serverMSPT <= 50 ? 20D : (1000D / this.serverMSPT);
            this.serverTPSValid = true;
        }
    }

    public void handleCarpetServerTPSData(Component textComponent)
    {
        if (textComponent.getFormattedText().isEmpty() == false)
        {
            String text = ChatFormat.stripFormatting(textComponent.getString());
            String[] lines = text.split("\n");

            for (String line : lines)
            {
                Matcher matcher = PATTERN_CARPET_TPS.matcher(line);

                if (matcher.matches())
                {
                    try
                    {
                        this.serverTPS = Double.parseDouble(matcher.group("tps"));
                        this.serverMSPT = Double.parseDouble(matcher.group("mspt"));
                        this.serverTPSValid = true;
                        this.carpetServer = true;
                        return;
                    }
                    catch (NumberFormatException e)
                    {
                    }
                }
            }
        }

        this.serverTPSValid = false;
    }
}
