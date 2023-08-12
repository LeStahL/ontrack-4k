#version 450

out vec4 out_color;

layout(location = 0) uniform float iTime;
layout(location = 1) uniform int iFrame;
layout(location = 2) uniform vec2 iResolution;
layout(location = 3) uniform int iPass;
layout(location = 4) uniform sampler2D iChannel0;

const vec3 c = vec3(1,0,-1);
const float pi = 3.14159;

// Created by David Hoskins and licensed under MIT.
// See https://www.shadertoy.com/view/4djSRW.
// float->float hash function
float hash11(float p)
{
    p = fract(p * .1031);
    p *= p + 33.33;
    p *= p + p;
    return fract(p);
}

// Created by David Hoskins and licensed under MIT.
// See https://www.shadertoy.com/view/4djSRW.
// vec2->float hash function
float hash12(vec2 p)
{
	vec3 p3  = fract(vec3(p.xyx) * .1031);
    p3 += dot(p3, p3.yzx + 33.33);
    return fract((p3.x + p3.y) * p3.z);
}

const mat3 Msrgb = mat3(
    0.4124564, 0.2126729, 0.0193339,
    0.3575761, 0.7151522, 0.1191920,
    0.1804375, 0.0721750, 0.9503041
), M1 = mat3(
    0.8189330101, 0.0329845436, 0.0482003018,
    0.3618667424, 0.9293118715, 0.2643662691,
    -0.1288597137, 0.0361456387, 0.6338517070
), M2 = mat3(
    0.2104542553, 1.9779984951, 0.0259040371,
    0.7936177850, -2.4285922050, 0.7827717662,
    -0.0040720468, 0.4505937099, -0.8086757660
);

// Convert rgb to xyz (sRGB) - compare http://www.brucelindbloom.com/index.html?Eqn_RGB_XYZ_Matrix.html
vec3 rgb2xyz_srgb(vec3 rgb) {
    return Msrgb * rgb;
}

// Convert xyz to rgb (sRGB) - compare http://www.brucelindbloom.com/index.html?Eqn_RGB_XYZ_Matrix.html
vec3 xyz2rgb_srgb(vec3 xyz) {
    return inverse(Msrgb) * xyz;
}

// Convert xyz to oklab - compare https://bottosson.github.io/posts/oklab/
vec3 xyz2oklab(vec3 xyz) {
    return M2 * pow(M1 * xyz, c.xxx/3.);
}

// Convert oklab to xyz - compare https://bottosson.github.io/posts/oklab/
vec3 oklab2xyz(vec3 lab) {
    return inverse(M1) * pow(inverse(M2) * lab, 3.*c.xxx);
}

// Convert oklab to oklch - compare https://bottosson.github.io/posts/oklab/
vec3 oklab2oklch(vec3 lab) {
    return vec3(lab.x, length(lab.yz), atan(lab.z, lab.y));
}

// Convert oklch to oklab - compare https://bottosson.github.io/posts/oklab/
vec3 oklch2oklab(vec3 lch) {
    return vec3(lch.x, lch.y * vec2(cos(lch.z), sin(lch.z)));
}

/* brownish clay
const int COLOR_COUNT = 5;

vec3 gradientColors[] = vec3[](
        vec3(0.86,0.86,0.91),
        vec3(0.67,0.64,0.71),
        vec3(0.27,0.22,0.28),
        vec3(0.43,0.25,0.32),
        vec3(0.59,0.40,0.46)
); */

/*
const int COLOR_COUNT = 5;
vec3 gradientColors[] = vec3[](
        vec3(0.02,0.84,0.63),
        vec3(0.96,0.96,1.00),
        vec3(1.00,0.82,0.39),
        vec3(0.29,0.39,0.43),
        vec3(0.04,0.08,0.10)
);*/
/*
const int COLOR_COUNT = 5;
vec3 gradientColors[] = vec3[](
        vec3(0.20,0.07,0.18),
        vec3(0.25,0.25,0.32),
        vec3(0.38,0.54,0.52),
        vec3(0.87,0.76,0.62),
        vec3(0.77,0.16,0.25)
);*/
const int COLOR_COUNT = 8;
vec3 gradientColors[] = vec3[](
        c.yyy,
        vec3(0.02,0.16,0.24),
        vec3(0.03,0.32,0.44),
        vec3(0.47,0.68,0.73),
        vec3(0.67,0.16,0.12),
        vec3(0.96,0.52,0.00),
        vec3(0.99,0.78,0.31),
        vec3(1.00,0.92,0.80)
);


vec3 weightedOklabLinearGradientOklab(float amount) {
    amount = fract(amount);
    // First rescale amount to match the distance in the color space.
    float colorspaceDistances[COLOR_COUNT],
        steps[COLOR_COUNT];
    float totalColorspaceDistance = 0.;
    for(int i=0; i<COLOR_COUNT; ++i) {
        vec3 c1 = (xyz2oklab(rgb2xyz_srgb(gradientColors[(i+1) % COLOR_COUNT]))),
            c2 = (xyz2oklab(rgb2xyz_srgb(gradientColors[i])));
        //colorspaceDistances[i] = abs(c1.x-c2.x) + length(c1.yz-c2.yz);
        colorspaceDistances[i] = length(c1 - c2);
        totalColorspaceDistance += colorspaceDistances[i];
    }
    
    // Normalize weights
    float currentStep = 0.;
    for(int i=0; i<COLOR_COUNT; ++i) {
        colorspaceDistances[i] /= totalColorspaceDistance;
        steps[i] = currentStep;
        currentStep += colorspaceDistances[i];
    }
    
    // Determine color mixing
    for(int i=0; i<COLOR_COUNT; ++i) {
        if(amount < steps[(i + 1) % COLOR_COUNT]) {
            return xyz2rgb_srgb(oklab2xyz((
                mix(
                    (xyz2oklab(rgb2xyz_srgb(gradientColors[i % COLOR_COUNT]))),
                    (xyz2oklab(rgb2xyz_srgb(gradientColors[(i+1) % COLOR_COUNT]))),
                    (amount-steps[i % COLOR_COUNT])/(steps[(i+1) % COLOR_COUNT] - steps[i % COLOR_COUNT])
                )
            )));
        }
    }
    
    return xyz2rgb_srgb(oklab2xyz((
        mix(
            (xyz2oklab(rgb2xyz_srgb(gradientColors[COLOR_COUNT - 1]))),
            (xyz2oklab(rgb2xyz_srgb(gradientColors[0]))),
            abs(amount-steps[COLOR_COUNT - 1])/abs(1.-steps[COLOR_COUNT - 1])
        )
    )));
}

vec3 lpos1 = vec3(0), 
    lpos2=vec3(0), 
    lpos3=vec3(0,1,0), 
    scatter =vec3(0);

void dmin(inout vec4 d, float x, float y, float z, float w)
{
	if( x < d.x ) d = vec4(x, y, z, w);
}

// 3D noise function (IQ)
float noise(vec3 p)
{
	vec3 ip=floor(p),
        s=vec3(7, 157, 113);
	p-=ip;
	vec4 h=vec4(0, s.yz, s.y+s.z)+dot(ip, s);
	p=p*p*(3.-2.*p);
	h=mix(fract(sin(h)*43758.5), fract(sin(h+s.x)*43758.5), p.x);
	h.xy=mix(h.xz, h.yw, p.y);
	return mix(h.x, h.y, p.z);
}

// method by fizzer
vec3 hashHs(vec3 n, float seed)
{
    float u = hash11( 78.233 + seed),
        v = hash11( 10.873 + seed),
        a = 6.2831853 * v;
    u = 2.0*u - 1.0;
    return normalize( n + vec3(sqrt(1.0-u*u) * vec2(cos(a), sin(a)), u) );
}

// rotation
void pR(inout vec2 p, float a)
{
	p = cos(a)*p+sin(a)*vec2(p.y, -p.x);
}

float m(vec2 x)
{
    return max(x.x,x.y);
}

float d210(vec2 x)
{
    return min(max(max(max(max(min(max(max(m(abs(vec2(abs(abs(x.x)-.25)-.25, x.y))-vec2(.2)), -m(abs(vec2(x.x+.5, abs(abs(x.y)-.05)-.05))-vec2(.12,.02))), -m(abs(vec2(abs(x.x+.5)-.1, x.y-.05*sign(x.x+.5)))-vec2(.02,.07))), m(abs(vec2(x.x+.5,x.y+.1))-vec2(.08,.04))), -m(abs(vec2(x.x, x.y-.04))-vec2(.02, .08))), -m(abs(vec2(x.x, x.y+.1))-vec2(.02))), -m(abs(vec2(x.x-.5, x.y))-vec2(.08,.12))), -m(abs(vec2(x.x-.5, x.y-.05))-vec2(.12, .07))), m(abs(vec2(x.x-.5, x.y))-vec2(.02, .08)));
}

float datz(vec2 uv)
{
    vec2 a = abs(uv)-.25;
    return max(max(min(max(min(abs(mod(uv.x-1./12.,1./6.)-1./12.)-1./30., abs(a.x+a.y)-.015),a.x+a.y), max(a.x+.1,a.y+.1)), -length(uv-vec2(0.,.04))+.045), -max(a.x+.225,a.y+.175));
}

const float f = 1.e4;

// Created by David Hoskins and licensed under MIT.
// See https://www.shadertoy.com/view/4djSRW.
// float->vec2 hash function
vec2 hash21(float p)
{
	vec3 p3 = fract(vec3(p) * vec3(.1031, .1030, .0973));
	p3 += dot(p3, p3.yzx + 33.33);
    return fract((p3.xx+p3.yz)*p3.zy);

}

vec2 uva;
vec3 palette(float scale)
{
    if(scale<=0.)
        return mix(vec3(0.96,0.26,0.07), vec3(1.), clamp(scale+1.,0.,1.));
    if(scale <= 1.)
        return mix(vec3(0.21,0.30,0.42), vec3(1.), fract(scale));
    if(scale <= 2.)
        return mix(vec3(0.21,0.42,0.30), vec3(0.21,0.30,0.42), fract(scale));
    return mix(vec3(1.00,0.94,0.41), vec3(0.21,0.30,0.42), fract(scale));
}

float sdTorus( vec3 p, vec2 t )
{
  vec2 q = vec2(length(p.xz)-t.x,p.y);
  return length(q)-t.y;
}

float lfnoise(vec2 t)
{
    vec2 i = floor(t);
    t = fract(t);
    t = smoothstep(c.yy, c.xx, t);
    vec2 v1 = vec2(hash12(i), hash12(i+c.xy)), 
        v2 = vec2(hash12(i+c.yx), hash12(i+c.xx));
    v1 = c.zz+2.*mix(v1, v2, t.y);
    return mix(v1.x, v1.y, t.x);
}

float tj,
    dt,
    bpm = 130.,
    spb,
    nbeats,
    scale,
    stepTime,
    hardBeats,
    expandRings,
    sineExpandRings,
    ringHeight,
    colorEscalation,
    glowEscalation,
    colorCandyNess,
    flashBackground;
vec4 map(vec3 p,bool flag)
{
    vec4 d = vec4(1),
        q;
    
    // point lights
    float re = min(length(p-lpos1), length(p-lpos2)) - .003;
    dmin(d, re, 1.,0.,.8);
    if(flag) scatter += max(-(re-.7),0.) * .49 * weightedOklabLinearGradientOklab(.6);

    // Outer Silo
    float pxs = 1.2;
    float x = mod(p.x, pxs)-.5*pxs,
        xj = p.x - x;
    float db = sdTorus(vec3(p.y, x, p.z), vec2(6., .5*pxs));
    dmin(d, db, 0., 0., 0.);
    if(flag) scatter += max(-(db-1.1),0.) * .26 * flashBackground * .5 * c.xxx * step(.5, hash12(vec2(xj, tj) + .5));

    p.y += .4 * iTime;
        
    // Silo
    float ps = (.1 + .9 * ringHeight) * (.05 + .95 * hash11(tj + .1));
    float y = mod(p.y, ps)-.5*ps,
        yj = p.y - y+13.3;
        
    float da = sdTorus(vec3(p.x, y, p.z), vec2(1. + expandRings * hash12(vec2(yj, tj) + .2) + 2. * sineExpandRings * lfnoise(vec2(yj, tj) + .2), .5*ps));
    da = min(da, sdTorus(vec3(p.x, y-ps, p.z), vec2(1. + expandRings * hash12(vec2(yj, tj) + .2)  + 2. * sineExpandRings * lfnoise(vec2(yj + ps, tj) + .2), .5*ps)));
    da = min(da, sdTorus(vec3(p.x, y+ps, p.z), vec2(1. + expandRings * hash12(vec2(yj, tj) + .2)  + 2. * sineExpandRings * lfnoise(vec2(yj - ps, tj) + .2), .5*ps)));
    float c1 = colorEscalation * step(hash12(vec2(yj, tj) + .1),.2 + .8 * colorCandyNess) * hash12(vec2(yj, tj) + .4);
    dmin(d, da, 0., 0., c1);
    if(flag) scatter += max(-(da-1.1),0.) * .26 * glowEscalation * .5 * mix(weightedOklabLinearGradientOklab(c1), c.yyy, 1.-scale) * scale;
    
    // Center of silo
    float dc = length(p.xz) - .03;
    dmin(d, dc, 1., 0., .8);
    if(flag) scatter += max(-(dc-.7),0.) * .49 * weightedOklabLinearGradientOklab(.6);

    // Floor
    
	return d;
}

vec3 normal(vec3 p)
{
	vec2 e = vec2(0, .0001);
	return normalize(map(p,false).x-vec3(map(p - e.yxx,false).x, map(p - e.xyx,false).x, map(p - e.xxy,false).x));
}

vec3 radiance(
    vec3 n,		// macro surface normal
    vec3 l,		// direction from vertex to light
    vec3 v,		// direction from vertex to view
    // matt
    float m	// roughness
) {
    vec3 cdiff = vec3(.8);
    vec3 cspec = vec3(.7);
    vec3 clight = vec3(.7, .65, .8);

    // half vector
    vec3 h = normalize( l + v );

    // dot
    float dot_n_h = max( dot( n, h ), .01 );
    float dot_n_v = max( dot( n, v ), .01 );
    float dot_n_l = max( dot( n, l ), .1 );
    float dot_h_v = max( dot( h, v ), .01 ); // dot_h_v == dot_h_l

    // Geometric Term
    // Cook-Torrance
    //          2 * ( N dot H )( N dot L )    2 * ( N dot H )( N dot V )
    // min( 1, ----------------------------, ---------------------------- )
    //                 ( H dot V )                   ( H dot V )
    float g = 2. * dot_n_h / dot_h_v;
    float G = min( min( dot_n_v, dot_n_l ) * g, 1. );

    // Normal Distribution Function ( cancel 1 / pi )
     // Beckmann distribution
    //         ( N dot H )^2 - 1
    //  exp( ----------------------- )
    //         ( N dot H )^2 * m^2
    // --------------------------------
    //         ( N dot H )^4 * m^2
    float sq_nh   = dot_n_h * dot_n_h;
    float sq_nh_m = sq_nh * ( m * m );
    float D = exp( ( sq_nh - 1. ) / sq_nh_m ) / ( sq_nh * sq_nh_m );

    // Specular Fresnel Term : Schlick approximation
    // F0 + ( 1 - F0 ) * ( 1 - ( H dot V ) )^5
    vec3 Fspec = cspec + ( 1.  - cspec ) * pow( 1. - dot_h_v, 5. );

    // Diffuse Fresnel Term : violates reciprocity...
    // F0 + ( 1 - F0 ) * ( 1 - ( N dot L ) )^5
    vec3 Fdiff = cspec + ( 1.  - cspec ) * pow( 1. - dot_n_l, 5. );

    // Cook-Torrance BRDF
    //          D * F * G
    // ---------------------------
    //  4 * ( N dot V )( N dot L )
    vec3 brdf_spec = Fspec * D * G / ( dot_n_v * dot_n_l * 4. );

    // Lambertian BRDF ( cancel 1 / pi )
    vec3 brdf_diff = cdiff * ( 1. - Fdiff );

    // Punctual Light Source ( cancel pi )
    return ( brdf_spec + brdf_diff ) * clight * dot_n_l;
}

void mainImage(out vec4 fragColor, in vec2 fragCoord)
{
    spb = 60. / bpm;
    stepTime = mod(iTime+.5*spb, spb)-.5*spb;
    nbeats = (iTime-stepTime+.5*spb)/spb + smoothstep(-.1*spb, .1*spb, stepTime);
    scale = smoothstep(-.1*spb, 0., stepTime)*smoothstep(.7*spb, 0., stepTime);
    hardBeats = round((iTime-mod(iTime, spb))/spb);
    dt = mod(iTime, spb);
    tj = iTime - dt;
    
    colorEscalation = smoothstep(4., 8., nbeats);
    glowEscalation = smoothstep(8., 12., nbeats);
    colorCandyNess = smoothstep(12., 16., nbeats);
    ringHeight = smoothstep(16., 20., nbeats) * (1. - .9*smoothstep(32., 36., nbeats));
    sineExpandRings = smoothstep(20., 24., nbeats) * (1. - smoothstep(40., 44., nbeats));
    expandRings = smoothstep(24., 28., nbeats) * (1. - smoothstep(36., 40., nbeats));
    flashBackground = smoothstep(28., 32., nbeats);
    
    fragColor = c.yyyx;
	vec2 uv = fragCoord.xy / iResolution.xy;

	// borders :(
	//if(uv.y>.11 && uv.y<.89)
	{
    	
		
        // camera
		vec3 ro1 = vec3(0, 0, -5.), rd1 = normalize(vec3((2.*fragCoord.xy-iResolution.xy)/iResolution.x, 1));
		float t1 = 0., t2 = 0.,t3=0., seed = 0.;
        // rotate scene
		//pR(rd1.xz,-.05*sin(.17*time));
		//pR(rd1.yz, .02*sin(.19*time));

//***************************************************************************************************
// Cast ray
//***************************************************************************************************     
  
        //lpos1 = ro1;
        lpos1 = - 4.*c.yyx+.3*vec3(1.*cos(iTime), .3*sin(iTime), .5+sin(.5*iTime));				// position point light 1 
        lpos2 = - 4.*c.yyx+.3*vec3(1.2*cos(.3*iTime), .4*sin(.4*iTime), .5+sin(.2*iTime));   	// position point light 2
       
        seed=hash12(uv + 13.)+fract(float(iFrame)*1.e-3) + 137.;
       // seed = (fragCoord.x+fragCoord.y);//+fract(iTime);   

		vec3 scol=vec3(0);
        vec4 m1, m2, m3;
        vec3 ro2,rd2,nor1, nor2,pos1,pos2,pos3;
        
        float roughness = .2;
	    
		for( int i = 0; i < 250; i++ )
		{
            // every iteration a new seed
            seed=32.+seed*fract(seed);
           	
            // raymarch 1st pass
           	pos1 = ro1+rd1*t1;		
        	m1 = map(pos1,true);
			t1+=0.2*(m1.x); 	// the smaller the factor, the thicker the media
			
            if (m1.x<.001)		// hit
            {
            	//scol+= palette(m1.z)*step(1., m1.y)*50.;
                
                nor1 = normal(pos1); 
        
                // cook torrance
                
                float as = .3,
                    ae = mod(pos1.x, as) - .5,
                    aej = (pos1.x - ae);
                
                scol += 5.e1 * weightedOklabLinearGradientOklab(m1.w) * (.3 + .1 * radiance(nor1, normalize(lpos1-pos1), normalize(ro1-pos1), roughness));
                scol += 5.e1 * weightedOklabLinearGradientOklab(m1.w) * (.3 + .1 * radiance(nor1, normalize(lpos2-pos1), normalize(ro1-pos1), roughness));

              	break;
            }

          
			// Raymarch direct light 
            //lpos3=vec3(sin(.5*iTime),1,0);				// light from ceiling 
            lpos3 = vec3(0.,1.,0.);
            lpos3 = mix(lpos3,hashHs(lpos3,seed),.15); 	// add randomness
        	pos2=pos1+lpos3*t2;							// calculate ray direction
            m2 = map(pos2,false);
            t2+=m2.x;
            
           	if (m2.y>=1.&&m2.z==0.) scol+= .5 +5.*m1.x*noise(7.*pos1+iTime);	// if ceiling hit
        }
        
      
	  	nor1 = normal(pos1); 
        

        
        // bounce        
        m1.y=clamp(m1.y,0.,1.);
		t2=0.;
       	for( int i = 1; i < 100 ; i++ )
      	{
            // every iteration a new seed
            seed=32.+seed*fract(seed);            
			rd2 = mix(reflect(rd1,nor1),hashHs(nor1,seed),m1.y);	// reflect depending on material	
            pos2 = pos1+ rd2*t2;
			m2 = map(pos2,false); 
            t2+=.2*m2.x;
            scol += (.4 /*+ .6 * scale*/) * c.xxx*step(1., m2.y);
           
            
        }
        
        if (m2.x<.001) {
            nor2 = normal(pos2);
            
            scol += 5.e1 * weightedOklabLinearGradientOklab(m2.w) * (.3 + .1* radiance(nor2, normalize(lpos1-pos2), normalize(ro1-pos2), .5* roughness));
            scol += 5.e1 * weightedOklabLinearGradientOklab(m2.w) * (.3 + .1* radiance(nor2, normalize(lpos2-pos2), normalize(ro1-pos2), .5* roughness));
        }
        
        

//		fragColor =vec4(scol*0.01+0.*scatter,0); //without blur
		fragColor = vec4((.003*scol+0.02*scatter+1.*texture(iChannel0, uv).xyz), 0.)*.8; // with blur
        
        fragColor = clamp(fragColor,0.,1.);
	}
}


void post( out vec4 fragColor, in vec2 fragCoord )
{
    fragColor = texture(iChannel0, fragCoord.xy/iResolution.xy);
}

void main() {
    if(iPass == 0) mainImage(out_color, gl_FragCoord.xy);
    else post(out_color, gl_FragCoord.xy);
}
