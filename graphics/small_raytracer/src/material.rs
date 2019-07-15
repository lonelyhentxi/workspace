use crate::{
    hitable::HitRecord,
    ray::Ray,
    vec3::{random_int_unit_sqhere, Vec3},
};
use rand::prelude::*;

#[derive(Copy, Clone)]
pub struct Scatter {
    pub attenuation: Vec3,
    pub ray: Ray,
}

impl Scatter {
    pub fn new(attenuation: Vec3, ray: Ray) -> Scatter {
        Scatter { attenuation, ray }
    }
}

#[derive(Copy, Clone)]
pub struct Lambertian {
    pub albedo: Vec3,
}

impl Lambertian {
    pub fn scatter(self, _: Ray, hit: HitRecord, rng: &mut ThreadRng) -> Scatter {
        let target = hit
    }
}
