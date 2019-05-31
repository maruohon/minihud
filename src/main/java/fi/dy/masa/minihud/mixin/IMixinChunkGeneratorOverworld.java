package fi.dy.masa.minihud.mixin;

import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.gen.Accessor;
import net.minecraft.world.gen.ChunkGeneratorOverworld;
import net.minecraft.world.gen.structure.MapGenScatteredFeature;
import net.minecraft.world.gen.structure.MapGenStronghold;
import net.minecraft.world.gen.structure.MapGenVillage;
import net.minecraft.world.gen.structure.StructureOceanMonument;
import net.minecraft.world.gen.structure.WoodlandMansion;

@Mixin(ChunkGeneratorOverworld.class)
public interface IMixinChunkGeneratorOverworld
{
    @Accessor("oceanMonumentGenerator")
    StructureOceanMonument getOceanMonumentGenerator();

    @Accessor("scatteredFeatureGenerator")
    MapGenScatteredFeature getScatteredFeatureGenerator();

    @Accessor("strongholdGenerator")
    MapGenStronghold getStrongholdGenerator();

    @Accessor("villageGenerator")
    MapGenVillage getVillageGenerator();

    @Accessor("woodlandMansionGenerator")
    WoodlandMansion getWoodlandMansionGenerator();
}
