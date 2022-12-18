package minihud.util;

import net.minecraft.util.math.BlockPos;
import net.minecraft.util.text.ITextComponent;
import net.minecraft.util.text.TextComponentTranslation;

import malilib.overlay.message.MessageUtils;
import minihud.MiniHud;
import minihud.config.Configs;
import minihud.data.DataStorage;
import minihud.data.DroppedChunks;

public class ChatUtils
{
    public static boolean onSendChatMessage(String message)
    {
        String[] parts = message.split(" ");

        if (parts[0].equals("minihud-seed"))
        {
            if (parts.length == 2)
            {
                try
                {
                    long seed = Long.parseLong(parts[1]);
                    DataStorage.INSTANCE.setWorldSeed(seed);
                    MessageUtils.printCustomActionbarMessage("minihud.message.info.seed_set", seed);
                }
                catch (NumberFormatException e)
                {
                    MessageUtils.printCustomActionbarMessage("minihud.message.error.failed_to_parse_seed_from_chat");
                }
            }
            else if (DataStorage.INSTANCE.hasStoredWorldSeed() && parts.length == 1)
            {
                MessageUtils.printCustomActionbarMessage("minihud.message.info.seed_set",
                                                         DataStorage.INSTANCE.getStoredWorldSeed());
            }

            return true;
        }
        else if (parts[0].equals("minihud-dropped-chunks-hash-size"))
        {
            if (parts.length == 2)
            {
                try
                {
                    int size = Integer.parseInt(parts[1]);
                    Configs.Generic.DROPPED_CHUNKS_HASH_SIZE.setValue(size);
                    // Fetch it again from the config, to take the bounds clamping into account
                    MessageUtils.printCustomActionbarMessage("minihud.message.info.dropped_chunks_hash_size_set_to",
                                                             Configs.Generic.DROPPED_CHUNKS_HASH_SIZE.getIntegerValue());
                }
                catch (NumberFormatException e)
                {
                    MessageUtils.printCustomActionbarMessage("minihud.message.error.invalid_dropped_chunks_hash_size");
                }
            }
            else if (parts.length == 1)
            {
                MessageUtils.printCustomActionbarMessage("minihud.message.info.dropped_chunks_hash_size_get",
                                                         DroppedChunks.getDroppedChunksHashSize());
            }

            return true;
        }

        return false;
    }

    public static void onReceiveChatMessage(ITextComponent message)
    {
        if (message instanceof TextComponentTranslation)
        {
            TextComponentTranslation text = (TextComponentTranslation) message;

            // The vanilla "/seed" command
            if ("commands.seed.success".equals(text.getKey()))
            {
                try
                {
                    long seed = Long.parseLong(text.getFormatArgs()[0].toString());
                    DataStorage.INSTANCE.setWorldSeed(seed);
                    MiniHud.LOGGER.info("Received world seed from the vanilla /seed command: {}", seed);
                    MessageUtils.printCustomActionbarMessage("minihud.message.info.seed_set", seed);
                }
                catch (Exception e)
                {
                    MiniHud.LOGGER.warn("Failed to read the world seed from '{}'", text.getFormatArgs()[0], e);
                }
            }
            // The "/jed seed" command
            else if ("jed.commands.seed.success".equals(text.getKey()))
            {
                try
                {
                    long seed = Long.parseLong(text.getFormatArgs()[1].toString());
                    DataStorage.INSTANCE.setWorldSeed(seed);
                    MiniHud.LOGGER.info("Received world seed from the JED '/jed seed' command: {}", seed);
                    MessageUtils.printCustomActionbarMessage("minihud.message.info.seed_set", seed);
                }
                catch (Exception e)
                {
                    MiniHud.LOGGER.warn("Failed to read the world seed from '{}'", text.getFormatArgs()[1], e);
                }
            }
            else if ("commands.setworldspawn.success".equals(text.getKey()) && text.getFormatArgs().length == 3)
            {
                try
                {
                    Object[] o = text.getFormatArgs();
                    int x = Integer.parseInt(o[0].toString());
                    int y = Integer.parseInt(o[1].toString());
                    int z = Integer.parseInt(o[2].toString());

                    BlockPos spawn = new BlockPos(x, y, z);
                    DataStorage.INSTANCE.setWorldSpawn(spawn);

                    String spawnStr = String.format("x: %d, y: %d, z: %d", spawn.getX(), spawn.getY(), spawn.getZ());
                    MiniHud.LOGGER.info("Received world spawn from the vanilla /setworldspawn command: {}", spawnStr);
                    MessageUtils.printCustomActionbarMessage("minihud.message.info.spawn_set", spawnStr);
                }
                catch (Exception e)
                {
                    MiniHud.LOGGER.warn("Failed to read the world spawn point from '{}'", text.getFormatArgs(), e);
                }
            }
        }
    }
}
