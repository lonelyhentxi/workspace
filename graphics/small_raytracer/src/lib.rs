mod vec3;
mod ray;
mod hitable;
mod material;

#[derive(Copy, Clone)]
pub struct Sphere {
    pub center: Vec3,
    pub radius: f32,
    pub material: Material,
}

pub struct World {
    spheres: Vec<Sphere>
}

impl World {
    pub fn new(spheres: Vec<Sphere>) -> World {
        World { spheres }
    }
}

#[derive(Copy,Clone)]
pub struct Lambertian {
    pub albedo: Vec3,
}

#[derive(Copy, Clone)]
pub struct Metal {
    pub albedo: Vec3,
    pub fuzz: f32,
}

#[derive(Copy,Clone)]
pub struct Dielectirc {
    pub refraction_index: f32,
}

#[derive(Copy, Clone)]
pub enum Material {
    Dielectric(Dielectirc),
    Lambertian(Lambertian),
    Metal(Metal)
}

impl Material {
    pub fn lambertian(albedo: Vec3) -> Material {
        Material::Lambertian(Lambertian { albedo })
    }

    pub fn metal(albedo: Vec3, fuzz: f32) -> Material {
        Material::Metal(Metal {albedo, fuzz})
    }

    pub fn dielectric(refraction_index: f32) -> Material {
        Material::Dielectric(Dielectirc {refraction_index})
    }
}
