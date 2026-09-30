# FES-CF V3.2.0 — Türkçe kullanım notu

Bu depo, Çağlar Sözen ve Mervenur Sözen'in *Forecasting Forward Financial Tail-Risk Surfaces: Guarded Functional Models and Conformal Bands* çalışmasına ait R kodunu, seçili sonuçlarını ve güncel audit kayıtlarını içerir. Ana açıklama [README.md](README.md) dosyasındadır.

## Kullanılacak ana kod

[analysis/FES_CF_V3_2_0_FULL.R](analysis/FES_CF_V3_2_0_FULL.R), sonuçları üreten V3.2.0 kaynakla byte düzeyinde aynıdır. GitHub hazırlığı sırasında istatistiksel yöntem veya sonuçlar değiştirilmedi. Kaynaktaki eski sürüm/geçiş açıklamaları tarihsel notlardır; yeni sonuçları paylaşmak için analizi yeniden çalıştırmanız gerektiği anlamına gelmez. “Prespecified” ifadesi programda sabitlenen ayarları belirtir; bağımsız ön kayıt kanıtı olarak sunulmaz.

## Kurulum ve kontrol

Depo klasöründe terminal açın:

```bash
Rscript scripts/install_dependencies.R
python3 scripts/verify_artifacts.py
Rscript tests/FES_CF_V3_2_0_SELF_TEST.R analysis/FES_CF_V3_2_0_FULL.R
```

Windows'ta Python komutu `py -3` olabilir. Kurulum dosyası eksik R paketlerini CRAN'dan yükler; finansal analizi başlatmaz. Sentetik test, önceki 29 kontrollü doğrulamayı tekrar çalıştırır; piyasa verisi deneyinin yerine geçmez. `docs/validation/synthetic_validation_29.txt` önceki sentetik koşumun kaydıdır.

## Finansal analizi çalıştırma

Analiz için önceki dondurulmuş çıktı klasöründe şu dosyalar gereklidir:

- `rds/config.rds`
- `cache/adjusted_prices_primary_universe.rds`
- `cache/vix_adjusted.rds`

Bu üç girdi GitHub paketinde bulunmaz. Elinizdeki gerçek klasör yollarını kullanın:

```bash
Rscript scripts/run_pipeline.R "C:/research/frozen_previous_output" "C:/research/new_v320_output" --check-only
Rscript scripts/run_pipeline.R "C:/research/frozen_previous_output" "C:/research/new_v320_output"
```

İlk komut yalnız temel girdi/ayar/paket kontrolü yapar; ikinci komut FULL analizi başlatır. Girdi ve çıktı klasörleri farklı olmalıdır. Cache içerikleri ve çalışma kimliği için asıl kontroller ana kodda yapılır. Eksik frozen girdileri yeni Yahoo indirmesiyle aynı kabul etmeyin.

Sonuçlar zaten hazırsa GitHub'a yüklemek için bu komutları çalıştırmanız gerekmez. Mevcut dosyaları toplamak için `scripts/COLLECT_EXISTING_V320_OUTPUTS.R` kullanılabilir; modelleri eğitmez.

## Sonuçların kapsamı

46 sonuç CSV'si LaTeX tablolarından çıkarılmış yuvarlatılmış değerlerdir. Yedi audit CSV'si ise sağlanan orijinal hassasiyetleriyle korunur. Publication audit 24/24 TRUE bildirmiş, ayrı incelemede 67 karşılaştırma geçmiştir. En son veri setinden tüm modellerin bağımsız tekrar üretildiği iddia edilmiyor.

Tam arşiv için güncel config/session-info, frozen girdiler ve hashleri, tam tahmin/kayıp dizileri ve ayrıntılı gecikmeli ACI yolu ayrıca gereklidir. FES-CF her karşılaştırmada en iyi değildir; ana testte global residual ablation tam modelden daha iyi sonuç verir. Bulguların ayrıntıları İngilizce README ve [VALIDATION.md](docs/VALIDATION.md) içindedir.

## Yayımlama

Kod deposu: [caglarsozen/fes-cf](https://github.com/caglarsozen/fes-cf). Kod ve beraberindeki dokümantasyon MIT lisansıyla sunulmaktadır. Ayrı bir release DOI'si, etiketlenmiş sürüm veya makale kabulü iddia edilmiyor. MIT telif ve izin bildiriminin korunmasını gerektirir; akademik atıf bilgileri `CITATION.cff` içindedir. Makale ve hakem raporları bu kod deposuna dahil edilmedi.
